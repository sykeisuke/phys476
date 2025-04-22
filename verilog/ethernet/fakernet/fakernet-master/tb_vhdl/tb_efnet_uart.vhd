library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.ALL;
use ieee.std_logic_unsigned.ALL;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.all;

entity tb_efnet_uart is
end entity tb_efnet_uart;

architecture RTL of tb_efnet_uart is

  -- Should simulation continue?
  signal runsim : boolean := true;

  -- Clock
  signal clk : std_logic;

  constant period : time := 10 ns;

  -- UART speed
  signal i_bit_period : std_logic_vector(7 downto 0) := "11010000";

  -- Hardware interface
  signal i_data     : std_logic_vector(7 downto 0) := (others => '0');
  signal i_has_data : std_logic := '1';

  signal o_taken    : std_logic;
  signal o_tx       : std_logic;

  signal i_rx       : std_logic := '0';
  signal o_data     : std_logic_vector(7 downto 0);
  signal o_has_data : std_logic;

  -- Check
  signal data       : std_logic_vector(8 downto 0) := (others => '0');
  signal data_prev  : std_logic_vector(7 downto 0) := (others => '0');
  signal failed     : std_logic := '0';

begin

  i_data <= data(7 downto 0);

  tx : entity work.efnet_uart_tx
    port map (
      clk          => clk,
      i_bit_period => i_bit_period,
      i_data       => i_data,
      i_has_data   => i_has_data,
      o_taken      => o_taken,
      o_tx         => o_tx
      );

  i_rx <= o_tx;

  rx : entity work.efnet_uart_rx
    port map (
      clk          => clk,
      i_bit_period => i_bit_period,
      i_rx         => i_rx,
      o_data       => o_data,
      o_has_data   => o_has_data
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

  process(clk)
  begin
    if (rising_edge(clk)) then
      if (o_taken = '1') then
        data_prev <= data(7 downto 0);
        data <= data + '1';
      end if;
    end if;
  end process;

  process(clk)
  begin
    if (rising_edge(clk)) then
      if (o_has_data = '1') then
        -- We are still transmitting the prev word, have not taken the next.
        if (o_data /= data_prev) then
          failed <= '1';
        end if;

        if (data = "100010000") then
          -- Try at a higher speed.  We can change this after having
          -- received since the transmitter is just sending stop cycle.
          i_bit_period <= "00011101";
        end if;
      end if;
    end if;
  end process;

  -- Abort simulation after any failure detected.
  -- Done later to allow all detections to trip.
  process
  begin
    if runsim then
      wait for 2 ns;
      assert (failed = '0') report "FAILED" severity failure;
      wait for 8 ns;
    else
      wait;
    end if;
  end process;

end architecture RTL;
