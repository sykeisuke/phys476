library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.ALL;
use ieee.std_logic_unsigned.ALL;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.all;

use work.fnet_records.all;

entity tb_fnet_mdio is
end entity tb_fnet_mdio;

architecture RTL of tb_fnet_mdio is

  -- Should simulation continue?
  signal runsim : boolean := true;

  -- Clock
  signal clk : std_logic;

  constant period : time := 10 ns;

  -- Hardware interface
  signal mdc_out    : std_logic;
  signal mdc_ena    : std_logic;
  signal mdio_in    : std_logic := '0';
  signal mdio_out   : std_logic;
  signal mdio_ena   : std_logic;

  -- MDIO request and response
  signal mdio_a_req_data  : std_logic_vector(31 downto 0) :=
    "01" & "01" & "00100" & "01010" & "10" & "11110000" & "00001111"; -- write
  signal mdio_a_request   : std_logic := '1';
  signal mdio_a_resp_data : std_logic_vector(17 downto 0) := (others => '0');
  signal mdio_a_response  : std_logic := '0';

  signal mdio_b_req_data  : std_logic_vector(31 downto 0) :=
    "01" & "10" & "00100" & "01010" & "00" & "11111111" & "00000000"; -- read
  signal mdio_b_request   : std_logic := '1';
  signal mdio_b_resp_data : std_logic_vector(17 downto 0) := (others => '0');
  signal mdio_b_response  : std_logic := '0';

begin
  mdio : entity work.fnet_mdio
    port map (
      clk       => clk,
      mdc_out   => mdc_out,
      mdc_ena   => mdc_ena,
      mdio_in   => mdio_in,
      mdio_out  => mdio_out,
      mdio_ena  => mdio_ena,
      a_req_data  => mdio_a_req_data,
      a_request   => mdio_a_request,
      a_resp_data => mdio_a_resp_data,
      a_response  => mdio_a_response,
      b_req_data  => mdio_b_req_data,
      b_request   => mdio_b_request,
      b_resp_data => mdio_b_resp_data,
      b_response  => mdio_b_response
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
      mdio_in <= not mdio_in;
    -- slow_clock_tick <= '1';
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
