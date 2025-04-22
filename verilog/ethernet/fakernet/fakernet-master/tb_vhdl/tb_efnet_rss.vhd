library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.all;

entity tb_efnet_rss is
end entity tb_efnet_rss;

architecture RTL of tb_efnet_rss is

  -- Should simulation continue?
  signal runsim : boolean := true;

  constant width : integer := 16;

  -- Clock
  signal clk        : std_logic;
  signal eth_rx_clk : std_logic;

  signal cnt_rx_clk : unsigned(15 downto 0) := (others => '0');

  signal mode_gmii  : std_logic := '0';

begin
  rss : entity work.efnet_rx_speed_sense
    generic map(clk_freq => 100000000)
    port map (
      clk         => clk,
      eth_rx_clk  => eth_rx_clk,

      o_mode_gmii => mode_gmii
      );

  -- Drive the clock
  process
  begin
    if runsim then
      clk <= '1';
      wait for 5 ns;
      clk <= '0';
      wait for 5 ns;
    else
      wait;
    end if;
  end process;

  -- Drive the clock
  process
  begin
    if runsim then
      if (cnt_rx_clk < 1000) then
        eth_rx_clk <= '1';
        wait for 4.1 ns;   -- ~8 ns period - 125 MHz
        eth_rx_clk <= '0';
        wait for 4 ns;
      else
        eth_rx_clk <= '1';
        wait for 20.1 ns;  -- ~40 ns period - 25 MHz
        eth_rx_clk <= '0';
        wait for 20 ns;
      end if;
    else
      wait;
    end if;
  end process;

  process(eth_rx_clk)
  begin
    if (rising_edge(eth_rx_clk)) then
      cnt_rx_clk <= cnt_rx_clk + 1;
    end if;
  end process;

end architecture RTL;
