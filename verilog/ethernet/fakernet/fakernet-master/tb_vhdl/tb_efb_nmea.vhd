library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.all;

entity tb_efb_nmea is
end entity tb_efb_nmea;

architecture RTL of tb_efb_nmea is

  -- Should simulation continue?
  signal runsim : boolean := true;

  -- Clock
  signal clk : std_logic;

  constant period : time := 10 ns;

  -- Hardware interface
  signal i_data       : std_logic_vector(7 downto 0) := (others => '0');
  signal i_has_data   : std_logic := '1';

  signal o_seconds    : std_logic_vector(16 downto 0);
  signal o_days       : std_logic_vector(15 downto 0);
  signal o_valid_fix  : std_logic;
  signal o_has_t_data : std_logic;

  signal o_qerr          : std_logic_vector(15 downto 0);
  signal o_has_qerr_data : std_logic;

  signal o_leap_info     : std_logic_vector(1 downto 0);
  signal o_has_leap_info : std_logic := '0';

begin

  nmea_parse : entity work.efb_nmea_parse
    port map (
      clk          => clk,
      i_data       => i_data,
      i_has_data   => i_has_data,
      o_seconds    => o_seconds,
      o_days       => o_days,
      o_valid_fix  => o_valid_fix,
      o_has_t_data => o_has_t_data
      );

  ublox_parse : entity work.efb_ublox_parse
    port map (
      clk          => clk,
      i_data       => i_data,
      i_has_data   => i_has_data,
      o_qerr       => o_qerr,
      o_has_qerr_data => o_has_qerr_data,
      o_leap_info  => o_leap_info,
      o_has_leap_info => o_has_leap_info
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

  -- Read input file with data
  testprocess: process is
    variable row    : line;
    variable c      : character;
    variable l      : line;
    variable rowval : std_logic_vector(7 downto 0);
    variable gotval : boolean;
    variable str    : String(1 to 4);
  begin
    -- Hack: get a bit into the clock cycle, to set the data to hold.
    wait for 1 ns;
    -- Wait a few clock cycles to get rid of meta values.
    wait for 100 ns;

    while (not endfile(input)) loop
      readline(input,row);

      if (row'length >= 4 and row.all(1 to 4) = String'("HEX:")) then
        read (row, str);

        while (row'length >= 2) loop
          hread(row,rowval,gotval);
          if (not gotval) then
            exit;
          end if;

          -- write (l, String'("INPUT-HEX: "));
          -- hwrite (l, rowval);
          -- writeline (output, l);

          i_data <= rowval;

          i_has_data <= '1';
          wait for 10 ns;
          i_has_data <= '0';
          wait for 30 ns;
        end loop;

      else
        for i in 1 to row'length loop
          c := row(i);

          -- write (l, String'("INPUT-CHAR: "));
          -- write (l, c);
          -- writeline (output, l);

          i_data <= std_logic_vector(to_unsigned(character'pos(c), 8));

          i_has_data <= '1';
          wait for 10 ns;
          i_has_data <= '0';
          wait for 30 ns;
        end loop;

        -- Inject CR
        i_data <= "00001101";
        i_has_data <= '1';
        wait for 10 ns;
        i_has_data <= '0';
        wait for 30 ns;
        -- Inject LF
        i_data <= "00001010";
        i_has_data <= '1';
        wait for 10 ns;
        i_has_data <= '0';
        wait for 30 ns;
      end if;

      -- write (l, String'("INPUT: "));
      -- write (l, row);
      -- writeline (output, l);
      --
      wait for 100 ns;
    end loop;

    -- We wait a little until we terminate.
    wait for 100 ns;
    write (l, String'("END"));
    writeline (output, l);

    -- Terminate simulation.
    runsim <= false;
    wait;

    assert (FALSE) report "Simulation end." severity failure;
  end process testprocess;

  -- Write produced output network words.
  process
    variable l : line;
    variable iv : integer;
  begin
    if (runsim) then
      -- Hack: get into the clock cycle, to get the latched data.
      wait for 1 ns;
      if (o_has_t_data = '1') then
        write (l, String'("OUTPUT: "));
        iv := to_integer(unsigned(o_days));
        write (l, iv);
        write (l, String'(" d "));
        iv := to_integer(unsigned(o_seconds));
        write (l, iv);
        write (l, String'(" s "));
        iv := to_integer(unsigned'("0" & o_valid_fix));
        write (l, iv);
        writeline (output, l);
      end if;
      if (o_has_qerr_data = '1') then
        write (l, String'("QERR: "));
        iv := to_integer(signed(o_qerr));
        write (l, iv);
        writeline (output, l);
      end if;
      if (o_has_leap_info = '1') then
        write (l, String'("LEAP: "));
        iv := to_integer(signed(o_leap_info));
        write (l, iv);
        writeline (output, l);
      end if;
      wait for 9 ns;
    else
      wait;
    end if;
  end process;

  process(clk)
  begin
    if (rising_edge(clk)) then

    end if;
  end process;

  -- Abort simulation after any failure detected.
  -- Done later to allow all detections to trip.
  process
  begin
    if runsim then
      wait for 2 ns;
      -- assert (failed = '0') report "FAILED" severity failure;
      wait for 8 ns;
    else
      wait;
    end if;
  end process;

end architecture RTL;
