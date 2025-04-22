library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.all;

entity tb_efb_cfg_parse is
end entity tb_efb_cfg_parse;

architecture RTL of tb_efb_cfg_parse is

  -- Should simulation continue?
  signal runsim : boolean := true;

  -- Clock
  signal clk : std_logic;

  constant period : time := 10 ns;

  -- Hardware interface
  signal i_reset_parse : std_logic := '0';
  signal i_data        : std_logic_vector(7 downto 0) := (others => '0');
  signal i_has_data    : std_logic := '0';
  signal o_value       : std_logic_vector(47 downto 0);
  signal o_value_index : std_logic_vector(24 downto 0);
  signal o_has_value   : std_logic := '0';
  signal i_value_done  : std_logic := '0';
  signal i_value_used  : std_logic := '0';
  signal o_done        : std_logic := '0';
  signal o_fail        : std_logic := '0';
  signal o_fail_code   : std_logic_vector( 3 downto 0);

  signal prev_done : std_logic := '0';

begin

  cfg_parse : entity work.efb_text_config_parse
    port map (
      clk          => clk,
      i_reset_parse => i_reset_parse,
      i_data        => i_data,
      i_has_data    => i_has_data,
      o_value       => o_value,
      o_value_index => o_value_index,
      o_has_value   => o_has_value,
      i_value_done  => i_value_done,
      i_value_used  => i_value_used,
      o_done        => o_done,
      o_fail        => o_fail,
      o_fail_code   => o_fail_code
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
    -- variable rowval : std_logic_vector(7 downto 0);
    -- variable gotval : boolean;
    variable str    : String(1 to 4);
    variable expect_fail : boolean;
  begin
    -- Hack: get a bit into the clock cycle, to set the data to hold.
    wait for 1 ns;
    -- Wait a few clock cycles to get rid of meta values.
    wait for 100 ns;

    while (not endfile(input)) loop
      readline(input,row);

      if (row'length >= 10 and
          (row.all(1 to 10) = String'("RESET_PASS") or
           row.all(1 to 10) = String'("RESET_FAIL"))) then
        expect_fail := (row.all(1 to 10) = String'("RESET_FAIL"));
        read (row, str);

        -- Inject termination (could also be 0).
        i_data <= "11111111";
        i_has_data <= '1';
        wait for 10 ns;
        i_has_data <= '0';
        wait for 400 ns;

        assert (o_done = '1')
          report "Expected done at reset." severity failure;

        --if (expect_fail) then
        --  assert (o_fail = '1')
        --    report "Expected fail at reset." severity failure;
        --else
        --  assert (o_fail = '0')
        --    report "Expected no fail at reset." severity failure;
        --end if;

        writeline (output, l);
        write (l, String'("RESET-INPUT"));
        if (expect_fail) then
          write (l, String'("-FAIL"));
        else
          write (l, String'("-PASS"));
        end if;
        if (expect_fail /= (o_fail = '1')) then
          write (l, String'(" ** UNEXPECTED **"));
        end if;
        writeline (output, l);
        writeline (output, l);

        i_reset_parse <= '1';
        wait for 10 ns;
        i_reset_parse <= '0';

      else
        -- write (l, String'("INPUT: "));
        write (l, row.all);
        writeline (output, l);
        --

        for i in 1 to row'length loop
          c := row(i);

          -- write (l, String'("INPUT-CHAR: "));
          -- write (l, c);
          -- writeline (output, l);

          i_data <= std_logic_vector(to_unsigned(character'pos(c), 8));

          i_has_data <= '1';
          wait for 10 ns;
          i_has_data <= '0';
          wait for 400 ns;
        end loop;

        -- Inject LF
        i_data <= "00001010";
        i_has_data <= '1';
        wait for 10 ns;
        i_has_data <= '0';
        wait for 400 ns;

      end if;

      wait for 100 ns;
    end loop;

    -- Inject termination (could also be 0).
    i_data <= "11111111";
    i_has_data <= '1';
    wait for 10 ns;
    i_has_data <= '0';
    wait for 400 ns;

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
      i_value_done <= '0';
      i_value_used <= '0';
      if (o_has_value = '1') then
        write (l, String'("VALUE @ "));
        iv := to_integer(unsigned(o_value_index));
        write (l, iv);
        write (l, String'(" : "));
        hwrite (l, o_value);
        -- Also print integer version as long as bit 31 or above are not set.
        if (o_value(o_value'high downto 31) =
            (o_value'high downto 31 => '0')) then
          write (l, String'(" = "));
          iv := to_integer(unsigned(o_value));
          write (l, iv);
        end if;
        writeline (output, l);
        i_value_done <= '1';
        -- For index 77, we process, but do not report the value as accepted.
        if (unsigned(o_value_index) /= 77) then
          i_value_used <= '1';
        end if;
      end if;
      if (o_done = '1' and prev_done = '0') then
        if (o_fail = '0') then
          write (l, String'("DONE"));
        else
          write (l, String'("FAIL code="));
          iv := to_integer(unsigned(o_fail_code));
          write (l, iv);
        end if;
        writeline (output, l);
      end if;
      prev_done <= o_done;
      wait for 9 ns;
    else
      wait;
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
