library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.all;

use work.fnet_records.all;

entity tb_efnet_w2o is
end entity tb_efnet_w2o;

architecture RTL of tb_efnet_w2o is

  -- Should simulation continue?
  signal runsim : boolean := true;

  constant width : integer := 16;

  -- Clock
  signal clk_a : std_logic;
  signal clk_b : std_logic;
  signal clk_c : std_logic;

  constant period_a : time :=  8 ns;
  constant period_b : time := 10 ns;


  signal cnt_input_a  : std_logic_vector(width-1 downto 0) := (others => '1');
  signal cnt_a        : std_logic_vector(15 downto 0) := (others => '0');

  signal input_a      : std_logic_vector(width-1 downto 0) := (others => '0');
  signal in_has_a     : std_logic := '0';
  signal in_used_a    : std_logic := '0';

  signal cnt_b : std_logic_vector(15 downto 0) := (others => '0');

  signal output_b     : std_logic_vector(width-1 downto 0) := (others => '0');
  signal out_has_b    : std_logic := '0';
  signal out_want_b   : std_logic := '0';

  signal expect_b     : std_logic_vector(width-1 downto 0) := (others => '0');

  signal good_b       : std_logic := '0';

  -- signal random1_a    : unsigned(15 downto 0) := (others => '0');
  -- signal random1_b    : unsigned(15 downto 0) := (others => '0');

  signal eth_tx_en    : std_logic := '0';
  signal eth_txd      : std_logic_vector(7 downto 0) := (others => '0');

  signal eth_txd_nibble : std_logic_vector(3 downto 0) := (others => '0');

  signal out_fifo_word      : std_logic_vector(15 downto 0) :=
    std_logic_vector(to_unsigned(16#dead#,16));
  signal out_fifo_ena       : std_logic := '0';
  signal out_fifo_many      : std_logic := '0';
  signal out_fifo_taken     : std_logic := '0';

  signal clk : std_logic;
  signal eth_gtx_clk_buf : std_logic;

begin

  clk             <= clk_a;
  eth_gtx_clk_buf <= clk_b;

  tx_fifo : entity work.efnet_word_out_fifo
    port map(
      clk_for_in  => clk,
      clk_for_out => eth_gtx_clk_buf, -- clk

      in_word     => input_a(15 downto 0),
      in_ena      => in_has_a,
      in_taken    => in_used_a,

      out_ena     => out_fifo_ena,
      out_word    => out_fifo_word,
      out_many    => out_fifo_many,
      out_taken   => out_fifo_taken
      );

  w2o : entity work.efnet_word_to_octet
    port map (
      -- clk => clk_a,

      out_ena     => out_fifo_ena,
      out_word    => out_fifo_word,
      out_many    => out_fifo_many,
      out_taken   => out_fifo_taken,

      i_mode_gmii => '1',

      eth_gtx_clk => eth_gtx_clk_buf,
      eth_tx_en   => eth_tx_en,
      eth_txd     => eth_txd
      );

  eth_txd_nibble <= eth_txd(3 downto 0);

  -- Drive the clock
  process
  begin
    if runsim then
      clk_a <= '1';
      wait for 8 ns;
      clk_a <= '0';
      wait for 8 ns;
    else
      wait;
    end if;
  end process;

  -- Drive the clock
  process
  begin
    if runsim then
      clk_b <= '1';
      wait for 5 ns;
      clk_b <= '0';
      wait for 5 ns;
    else
      wait;
    end if;
  end process;

  -- Drive the clock
  process
  begin
    if runsim then
      clk_c <= '1';
      wait for 8 ns;
      clk_c <= '0';
      wait for 8 ns;
    else
      wait;
    end if;
  end process;

  process(clk_a)
  begin
    if (rising_edge(clk_a)) then
      if (in_used_a = '1') then
        cnt_a <= std_logic_vector(unsigned(cnt_a) + ("" & "1"));
        cnt_input_a <=
          std_logic_vector(unsigned(cnt_input_a) + ("" & "1"));
      end if;

      -- random1_a <= resize((random1_a * 75),16) + 74;
    end if;
  end process;

  in_has_a <=
    '1' when (cnt_a(5 downto 3) = "000") else -- Premable, SFD, Data
    '0';
  input_a <=
    "01010101"&"11010101" when (cnt_a(5 downto 0) = "000011") else -- Premable
    "01010101"&"01010101" when (cnt_a(5 downto 2) = "0000"  ) else -- SFD
    cnt_input_a           when (cnt_a(5 downto 2) = "0001"  ) else -- Data
    "10101010"&"10111011" when (cnt_a(5 downto 2) = "0010"  ) else -- IGP
    "10101010"&"10111010";                                         -- Idle

  process(clk_b)
  begin
    if (rising_edge(clk_b)) then
      cnt_b <= std_logic_vector(unsigned(cnt_b) + "1");

      if (cnt_b(0) = '1') then
        out_want_b <= '1';
      else
        out_want_b <= '0';
      end if;

      expect_b <= std_logic_vector(unsigned(expect_b) + ("" & out_has_b));

      if (out_has_b = '1') then
        if (expect_b = output_b) then
          good_b <= '1';
        else
          good_b <= '0';
        end if;
      end if;

      -- random1_b <= resize((random1_b * 75),16) + 74;
    end if;
  end process;

  -- Abort simulation after any failure detected.
  -- Done later to allow all detections to trip.
  process
  begin
    if runsim then
      wait for 2 ns;
      --assert (failed = '0') report "FAILED" severity failure;
      wait for 8 ns;
    else
      wait;
    end if;
  end process;

end architecture RTL;
