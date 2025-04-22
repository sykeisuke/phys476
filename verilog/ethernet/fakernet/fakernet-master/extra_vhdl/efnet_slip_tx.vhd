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

entity efnet_slip_tx is
  port (
    clk             : in  std_logic; -- Clock from board
    -- Input side.
    out_word        : in  std_logic_vector(15 downto 0);
    out_payload     : in  std_logic;
    out_crc         : in  std_logic;
    out_taken       : out std_logic := '0';
    -- Output side.
    o_data          : out std_logic_vector(7 downto 0);
    o_has_data      : out std_logic;
    -- Output signals.
    i_taken         : in  std_logic := '0'
    );

end efnet_slip_tx;

architecture RTL of efnet_slip_tx is

  subtype char  is unsigned(7 downto 0);

  type packet_state is (PACKET_IDLE,
                        SKIP_HEADER1,
                        SKIP_HEADER2,
                        SKIP_HEADER3,
                        SKIP_HEADER4,
                        SKIP_HEADER5,
                        SKIP_HEADER6,
                        SELECT_BYTE,
                        WAIT_ESCAPE_TAKEN,
                        WAIT_TAKEN,
                        CONSUME_WORD,
                        GET_WORD,
                        PACKET_CONTINUE,
                        WAIT_END_TAKEN);

  signal state      : packet_state := PACKET_IDLE;

  -- For generating characters.
  constant char_0xc0   : char := "11000000"; -- c0
  constant char_0xdb   : char := "11011011"; -- db
  constant char_0xdc   : char := "11011100"; -- dc
  constant char_0xdd   : char := "11011101"; -- dd

  -- Next byte is word2 (of 2).
  signal word2_next    : std_logic := '0';

  signal cur_byte      : char := (others => '0');

  signal byte          : char := (others => '0');
  signal has_byte      : std_logic := '0';

  signal cur_byte_is_esc1 : std_logic := '0';
  signal cur_byte_is_esc2 : std_logic := '0';

begin

  -- Feed the input history.
  -- input_history(0) <= unsigned(i_data);

  cur_byte <=
    char(out_word(15 downto  8)) when (word2_next = '0') else
    char(out_word( 7 downto  0));

  cur_byte_is_esc1 <= '1' when (cur_byte = char_0xc0) else '0';
  cur_byte_is_esc2 <= '1' when (cur_byte = char_0xdb) else '0';

  -- This state machine is not the most efficient, but that does not
  -- matter.  The UART TX will report the byte as taken at the
  -- beginning of its transmission, and we will therefore finish
  -- preparing the next byte long before it can be sent.

  process (clk)
  begin
    if (rising_edge(clk)) then

      -- Default values.
      out_taken <= '0';
      has_byte <= '0';
    
      case state is
      
        when PACKET_IDLE =>
          if (out_payload = '1' and out_crc = '0') then
            state <= SKIP_HEADER1;
          else
            out_taken <= '1';
          end if;
          -- Make sure to be in sync with output.
          word2_next <= '0';

          -- Skip the Ethernet header, one word already consumed.
        when SKIP_HEADER1 =>
          out_taken <= '1';
          state <= SKIP_HEADER2;
       
        when SKIP_HEADER2 =>
          out_taken <= '1';
          state <= SKIP_HEADER3;
       
        when SKIP_HEADER3 =>
          out_taken <= '1';
          state <= SKIP_HEADER4;
       
        when SKIP_HEADER4 =>
          out_taken <= '1';
          state <= SKIP_HEADER5;
       
        when SKIP_HEADER5 =>
          out_taken <= '1';
          state <= SKIP_HEADER6;
       
        when SKIP_HEADER6 =>
          out_taken <= '1';
          state <= GET_WORD;
       
        when SELECT_BYTE =>
          -- If the byte to be emitted needs an escape character,
          -- then first send that.
          if (cur_byte_is_esc1 = '1' or
              cur_byte_is_esc2 = '1') then
            byte <= char_0xdb;
            state <= WAIT_ESCAPE_TAKEN;
          else
            byte <= cur_byte;
            state <= WAIT_TAKEN;
          end if;

        when WAIT_ESCAPE_TAKEN =>
          if (i_taken = '1') then
            if (cur_byte_is_esc1 = '1') then
              byte <= char_0xdc;
            else
              byte <= char_0xdd;
            end if;
            state <= WAIT_TAKEN;
          else
            has_byte <= '1';
          end if;

        when WAIT_TAKEN =>
          if (i_taken = '1') then
            if (word2_next = '1') then
              -- We just consumed the second byte.
              state <= CONSUME_WORD;
            else
              -- Handle the second byte.
              state <= SELECT_BYTE;
            end if;
            word2_next <= not word2_next;
          else
            has_byte <= '1';
          end if;

        when CONSUME_WORD =>
          -- Get another word...
          out_taken <= '1';
          -- ... wait for one cycle to get the word ...
          -- (Due to use setting out_taken synchronously.)
          state <= GET_WORD;

        when GET_WORD =>
          -- ... then figure out how to continue.
          state <= PACKET_CONTINUE;

        when PACKET_CONTINUE =>
          if (out_crc = '1') then
            -- CRC is the end of the packet.  Send end token.
            byte <= char_0xc0;
            state <= WAIT_END_TAKEN;
          else
            -- Continue the packet.
            state <= SELECT_BYTE;
          end if;
        
        when WAIT_END_TAKEN =>
          if (i_taken = '1') then
            state <= PACKET_IDLE;
          else
            has_byte <= '1';
          end if;

      end case;
    end if;
  end process;

  -- Alias to output variables.
  o_data     <= std_logic_vector(byte);
  o_has_data <= has_byte;
  
end RTL;
