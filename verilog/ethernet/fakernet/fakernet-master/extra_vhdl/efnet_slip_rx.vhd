-- Copyright (c) 2023, Haakan T. Johansson
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the authors nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity efnet_slip_rx is
  port (
    clk             : in  std_logic; -- Clock from board
    -- Input signals.
    i_data          : in  std_logic_vector(7 downto 0);
    i_has_data      : in  std_logic;
    -- Output signals.
    o_word_1        : out std_logic_vector(7 downto 0); -- The octets that make
    o_word_2        : out std_logic_vector(7 downto 0); -- up the 16 bit word.
    o_words_ready   : out std_logic; -- High when full word has been received.
    o_packet_start  : out std_logic; -- High if SFD detected.
    o_packet_ended  : out std_logic  -- High for one cycle if ended.
    );

end efnet_slip_rx;

-- Heuristically determine if a data stream is SLIP (serial line IP),
-- works with 'negative' feedback:
--
-- - If the END (also packet start mark) is not seen every so often,
--   then it is not a packet stream.  This works also if the data does
--   not contain high (>= 128) bytes, since the start character is
--   0xc0.
--
-- - If the data stream does contain also high bytes, preferably all
--   kinds of bytes, then any escape byte (0xdb) shall be followed by
--   either 0xdc or 0xdd.  If not, then it is a failure.
--
-- The problem with the above checks is that they require a long
-- stream of data before they can rule out a stream being SLIP.
--
-- - A better check is the inspection of the IP header checksum.  That
--   occurs at a fixed offset from the start (END) character and thus
--   will give feedback for each packet.  Likewise, UDP and TCP has
--   another checksum, which also can be checked.  The feedback about
--   these values can be gotten from the input packet parser.
--
--   Except that the input packet parser currently does not handle
--   packets with IP number which are not destinied for us.  OTOH,
--   with SLIP, only such packets should be sent.
--
-- Neither of the above is implemented.

architecture RTL of efnet_slip_rx is

  subtype char  is unsigned(7 downto 0);

  type packet_state is (PACKET_IDLE,
                        PACKET_DATA,
                        PACKET_ESCAPE);

  signal state      : packet_state := PACKET_IDLE;
  signal next_state : packet_state := PACKET_IDLE;

  -- For identifying characters.
  constant char_0xc0   : char := "11000000"; -- c0
  constant char_0xdb   : char := "11011011"; -- db
  constant char_0xdc   : char := "11011100"; -- dc
  constant char_0xdd   : char := "11011101"; -- dd

  -- History of 1 recent digit.
  type char_history_array is array(0 downto 0) of char;
  signal input_history : char_history_array :=
    (others => (others => '0'));

  -- From async parser.
  signal parse_byte      : char := (others => '0');
  signal parse_has_byte      : std_logic := '0';
  signal parse_start_pending : std_logic := '0';
  signal parse_end           : std_logic := '0';
  signal parse_abort         : std_logic := '0';

  -- State:
  -- Next byte is a packet start.
  signal start_pending   : std_logic := '0';
  -- Next byte is word2 (of 2).
  signal word2_next      : std_logic := '0';

  -- Downcount of dummy words (ethernet header and footer).
  signal eth_cnt         : unsigned(2 downto 0) := (others => '0');
  signal eth_cnt_next    : unsigned(3 downto 0) := (others => '0');
  signal eth_cnt_wrap    : std_logic := '0';

  -- Downcount of dummy words for minimum frame size (64 octets / 32 words).
  signal mfs_cnt         : unsigned(4 downto 0) := (others => '0');
  signal mfs_cnt_next    : unsigned(5 downto 0) := (others => '0');
  signal mfs_cnt_wrap    : std_logic := '0';

  signal mfs_do_pad      : std_logic := '0';

  -- Sync parse output.
  signal word1           : char := (others => '0');
  signal word2           : char := (others => '0');
  signal has_eth         : std_logic := '0'; -- Fake ethernet frame.
  signal has_word        : std_logic := '0';
  signal has_start       : std_logic := '0';

begin

  -- Feed the input history.
  input_history(0) <= unsigned(i_data);

  process(state, input_history, start_pending)
  begin

    -- Default values.
    next_state <= PACKET_IDLE;

    parse_byte     <= input_history(0);
    parse_has_byte <= '0';

    parse_start_pending <= '0';
    parse_end      <= '0';
    parse_abort    <= '0';

    case state is

      when PACKET_IDLE =>
        if (input_history(0) = char_0xc0) then
          next_state <= PACKET_DATA;
          parse_start_pending <= '1';
        end if;

      when PACKET_DATA =>
        if    (input_history(0) = char_0xc0) then -- END
          next_state <= PACKET_DATA;
          -- No byte this cycle.  But start is pending.
          parse_start_pending <= '1';
          parse_end <= not start_pending;
        elsif (input_history(0) = char_0xdb) then -- ESC
          next_state <= PACKET_ESCAPE;
          -- No byte this cycle.
          parse_start_pending <= start_pending;
        else
          next_state <= PACKET_DATA;
          parse_has_byte <= '1';
        end if;

      when PACKET_ESCAPE =>
        if    (input_history(0) = char_0xdc) then -- ESC-END
          next_state <= PACKET_DATA;
          parse_byte <= char_0xc0;  -- Actual payload END.
          parse_has_byte <= '1';
        elsif (input_history(0) = char_0xdd) then -- ESC-ESC
          next_state <= PACKET_DATA;
          parse_byte <= char_0xdb; -- Actual payload ESC.
          parse_has_byte <= '1';
        else
          -- We detected an error!
          next_state <= PACKET_IDLE;
          parse_abort <= '1';
        end if;

    end case;
  end process;

  -- Do counting.
  eth_cnt_next <= ('0' & eth_cnt) - 1;
  eth_cnt_wrap <= eth_cnt_next(eth_cnt_next'left);

  mfs_cnt_next <= ('0' & mfs_cnt) - 1;
  mfs_cnt_wrap <= mfs_cnt_next(mfs_cnt_next'left);

  process (clk)
  begin
    if (rising_edge(clk)) then

      -- Default.
      has_word <= '0';
      has_start <= '0';
      has_eth <= '0';

      if (eth_cnt_wrap = '0') then
        -- Count as long as count is not 0.
        eth_cnt <= eth_cnt_next(eth_cnt'range);
        has_eth <= '1'; -- Header or footer.
      end if;
      -- While packet is processed, count number of words emitted.
      -- When packet is over, fill with padding (if needed).
      if ((mfs_do_pad = '1' or has_word = '1') and
          mfs_cnt_wrap = '0') then
        mfs_cnt <= mfs_cnt_next(mfs_cnt'range);
        has_eth <= mfs_do_pad;
      end if;

      -- Only process when new character is available.
      if (i_has_data = '1') then
        state <= next_state;

        -- Next (actual) byte is start.
        start_pending <= parse_start_pending;

        if (parse_has_byte = '1') then
          if (start_pending = '1' or
              word2_next = '0') then
            word2_next <= '1';
            word1 <= parse_byte;
          else
            -- Send a latched value.
            has_word <= '1';
            word2_next <= '0';
            word2 <= parse_byte;
          end if;
          -- Packet start is sent _before_ the first word.
          -- (has_word is set on the second byte)
          has_start <= start_pending;
          if (start_pending = '1') then
            eth_cnt <= to_unsigned(7, eth_cnt'length);
            mfs_cnt <= to_unsigned(32-7, mfs_cnt'length);
            mfs_do_pad <= '0';
          end if;
        end if;
        if (parse_end = '1') then
          eth_cnt <= to_unsigned(2, eth_cnt'length);
          mfs_do_pad <= '1';
        end if;
        if (parse_abort = '1') then
          mfs_do_pad <= '0';
        end if;
      end if;

      -- Final output preparation.
      if (has_word = '1') then
        -- Actual data,
        o_word_1 <= std_logic_vector(word1);
        o_word_2 <= std_logic_vector(word2);
      else
        -- Dummy word is the ethertype mark for IP packet.
        o_word_1 <= "00001000";
        o_word_2 <= "00000000";
      end if;
      o_words_ready  <= has_eth or has_word;
      o_packet_start <= has_start;
      o_packet_ended <= '0'; -- not used by anyone?
    end if;
  end process;

  -- Alias to output variables.

end RTL;
