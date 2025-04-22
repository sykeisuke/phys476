library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.all;

entity tb_efb_spi_read is
end entity tb_efb_spi_read;

architecture RTL of tb_efb_spi_read is

  -- Should simulation continue?
  signal runsim : boolean := true;

  -- Clock
  signal clk : std_logic;

  constant period : time := 10 ns;

  -- SPI interface (inputs).
  signal spi_sdi      : std_logic := '0';
  -- SPI interface (outputs).
  signal spi_csn      : std_logic;
  signal spi_sdo      : std_logic;
  signal spi_wpn      : std_logic;
  signal spi_hldn     : std_logic;
  signal spi_sck      : std_logic;

  -- Start a read sequence:
  signal i_addr       : std_logic_vector(23 downto 0) := (others => '0');
  signal i_start      : std_logic := '0';

  -- Output data (one byte).
  signal o_data       : std_logic_vector(7 downto 0);
  signal o_has_data   : std_logic;

  -- External counter to drive tests.
  signal count        : unsigned(11 downto 0) := (others => '0');

  signal spi_sck_prev : std_logic := '0';

  signal pattern      : std_logic_vector(31 downto 0) :=
    "01010011" & "01101001" & "11000011" & "10100101";

  signal ptn_cnt      : unsigned(4 downto 0) := (others => '0');

begin

  spi_read : entity work.efb_spi_flash_read
    port map (
      clk          => clk,

      spi_sdi      => spi_sdi,

      spi_csn      => spi_csn,
      spi_sdo      => spi_sdo,
      spi_wpn      => spi_wpn,
      spi_hldn     => spi_hldn,
      spi_sck      => spi_sck,

      i_addr       => i_addr,
      i_start      => i_start,

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
      count <= count + 1;

      i_start <= '0';
      if (to_integer(count) = 7) then
        i_start <= '1';
        i_addr <= "10011001" & "01100110" & "10100101";
      end if;

      -- Just toggle the data-in bit.
      -- Changed on falling edge of spi_clk.
      if (spi_sck = '0' and spi_sck_prev = '1') then
        ptn_cnt <= ptn_cnt + 1;
        spi_sdi <= pattern(to_integer(ptn_cnt));
      end if;
      spi_sck_prev <= spi_sck;
    end if;
  end process;

end architecture RTL;
