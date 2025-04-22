library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.all;

entity tb_efb_track_pps_ts is
end entity tb_efb_track_pps_ts;

architecture RTL of tb_efb_track_pps_ts is

  -- Should simulation continue?
  signal runsim : boolean := true;

  -- Clock
  signal clk : std_logic;

  constant period : time := 10 ns;

  -- Hardware interface
  signal i_ref_value : std_logic_vector(15 downto 0) := (others => '0');
  signal i_has_value : std_logic := '0';
  signal i_pps       : std_logic := '0';

  signal o_cur_value : std_logic_vector(15 downto 0);
  signal o_sync      : std_logic;

  -- Ticker to drive the PPS.
  signal ticker       : integer := 0;
  signal ticker_limit : integer := 66;

  signal emit_pps     : std_logic := '1';

  signal ref_value    : unsigned(19 downto 0) := (others => '0');
  signal cycle_count  : unsigned(19 downto 0) := (others => '0');

  signal nom_add_value    :
    std_logic_vector(7 downto 0) := "00000010"; --   2
  signal nom_add_frac_min :
    std_logic_vector(7 downto 0) := "11110000"; -- 240 (/256=.9375)
  signal corr_factor      :
    std_logic_vector(5 downto 0) := "000011";   -- 3
  signal nom_pps_clks     :
    std_logic_vector(5 downto 0) := "111111";   -- 63 (~[63..68]/2)

  signal ref_jump     : std_logic := '0'; -- Ref time jump.
  signal tc           : std_logic := '0'; -- Tick change.
  signal mpps         : std_logic := '0'; -- play with pps.

  signal cur_ref_diff : signed(15 downto 0) := (others => '0');

begin

  -- 100 MHz, 66.66 clock cycles per PPS
  -- We want the value to increase by 200 each PPS,
  -- So add should be 3 nominally.
  -- We start at 3-16/256 = 2.9375 and go to 3+48/256 = 3.1875,
  -- which allows 62.7 to 68.1 clock cycles per PPS.

  i_ref_value <= std_logic_vector(ref_value(i_ref_value'range));

  track_pps : entity work.efb_track_pps_ts
    generic map(
      frac_bits => 8,
      frac_add_bits => 6,
      diff_limit => 16,
      max_diff_bits => 5,
      pps_nsubsamples => 1,
      pps_subtime_bits => 0,
      pps_subtime_mul => 1,
      pps_subtime_div => 1,
      qerr_mul => 1
      )
    port map (
      clk          => clk,
      i_ref_value  => i_ref_value,
      i_has_value  => i_has_value,
      i_pps        => i_pps,

      i_nom_add_value    => nom_add_value,
      i_nom_add_frac_min => nom_add_frac_min,
      i_corr_factor      => corr_factor,
      i_nom_pps_clks     => nom_pps_clks,

      o_cur_value  => o_cur_value,
      o_sync       => o_sync
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
      i_pps <= '0';
      i_has_value <= '0';
      if (ticker = ticker_limit) then
        ref_value <= ref_value + to_unsigned(200, 19);
        cycle_count <= cycle_count + ("" & '1');
        i_has_value <= '1';
        i_pps <= emit_pps;
        ticker <= 1;

        ref_jump <= '0';
        tc       <= '0';
        mpps     <= '0';

        if (to_integer(cycle_count) = 150) then
          ref_value <= ref_value;                         ref_jump <= '1';
        end if;
        if (to_integer(cycle_count) = 200) then
          ref_value <= ref_value + to_unsigned(400, 19);  ref_jump <= '1';
        end if;

        if (to_integer(ref_value)    =  10000) then ticker_limit <= 67;tc<='1';
        elsif (to_integer(ref_value) =  20000) then ticker_limit <= 65;tc<='1';
        elsif (to_integer(ref_value) =  70000) then ticker_limit <= 65;tc<='1';
        elsif (to_integer(ref_value) =  80000) then ticker_limit <= 64;tc<='1';
        elsif (to_integer(ref_value) =  90000) then ticker_limit <= 62;tc<='1';
        elsif (to_integer(ref_value) = 100000) then ticker_limit <= 61;tc<='1';
        elsif (to_integer(ref_value) = 110000) then ticker_limit <= 60;tc<='1';
        elsif (to_integer(ref_value) = 120000) then ticker_limit <= 66;tc<='1';
        elsif (to_integer(ref_value) = 150000) then ticker_limit <= 63;tc<='1';
        elsif (to_integer(ref_value) = 160000) then ticker_limit <= 67;tc<='1';
        elsif (to_integer(ref_value) = 170000) then ticker_limit <= 63;tc<='1';
        elsif (to_integer(ref_value) = 180000) then ticker_limit <= 67;tc<='1';
        elsif (to_integer(ref_value) = 190000) then ticker_limit <= 63;tc<='1';
        elsif (to_integer(ref_value) = 200000) then ticker_limit <= 67;tc<='1';
        elsif (to_integer(ref_value) = 210000) then ticker_limit <= 63;tc<='1';
        elsif (to_integer(ref_value) = 220000) then ticker_limit <= 67;tc<='1';
        elsif (to_integer(ref_value) = 230000) then ticker_limit <= 63;tc<='1';
        elsif (to_integer(ref_value) = 240000) then ticker_limit <= 69;tc<='1';
        elsif (to_integer(ref_value) = 250000) then ticker_limit <= 65;tc<='1';
        elsif (to_integer(ref_value) = 260000) then ticker_limit <= 62;tc<='1';
        elsif (to_integer(ref_value) = 270000) then ticker_limit <= 63;tc<='1';
        elsif (to_integer(ref_value) = 280000) then ticker_limit <= 64;tc<='1';
        elsif (to_integer(ref_value) = 290000) then ticker_limit <= 65;tc<='1';
        elsif (to_integer(ref_value) = 300000) then ticker_limit <= 66;tc<='1';
        elsif (to_integer(ref_value) = 310000) then ticker_limit <= 67;tc<='1';
        elsif (to_integer(ref_value) = 320000) then ticker_limit <= 68;tc<='1';
        elsif (to_integer(ref_value) = 330000) then ticker_limit <= 69;tc<='1';
        elsif (to_integer(ref_value) = 340000) then ticker_limit <= 68;tc<='1';
        elsif (to_integer(ref_value) = 350000) then ticker_limit <= 67;tc<='1';
        elsif (to_integer(ref_value) = 360000) then ticker_limit <= 66;tc<='1';
        elsif (to_integer(ref_value) = 370000) then ticker_limit <= 65;tc<='1';
        elsif (to_integer(ref_value) = 380000) then ticker_limit <= 64;tc<='1';
        elsif (to_integer(ref_value) = 390000) then ticker_limit <= 63;tc<='1';
        elsif (to_integer(ref_value) = 400000) then ticker_limit <= 62;tc<='1';
        elsif (to_integer(ref_value) = 410000) then ticker_limit <= 67;tc<='1';
        elsif (to_integer(ref_value) = 420000) then ticker_limit <= 62;tc<='1';
        elsif (to_integer(ref_value) = 430000) then ticker_limit <= 66;tc<='1';
        elsif (to_integer(ref_value) = 440000) then ticker_limit <= 62;tc<='1';
        elsif (to_integer(ref_value) = 450000) then ticker_limit <= 65;tc<='1';
        elsif (to_integer(ref_value) = 460000) then ticker_limit <= 62;tc<='1';
        elsif (to_integer(ref_value) = 470000) then ticker_limit <= 64;tc<='1';
        elsif (to_integer(ref_value) = 480000) then ticker_limit <= 63;tc<='1';
        elsif (to_integer(ref_value) = 490000) then ticker_limit <= 68;tc<='1';
        elsif (to_integer(ref_value) = 500000) then ticker_limit <= 63;tc<='1';
        elsif (to_integer(ref_value) = 510000) then ticker_limit <= 67;tc<='1';
        elsif (to_integer(ref_value) = 520000) then ticker_limit <= 63;tc<='1';
        elsif (to_integer(ref_value) = 530000) then ticker_limit <= 66;tc<='1';
        elsif (to_integer(ref_value) = 540000) then ticker_limit <= 63;tc<='1';
        elsif (to_integer(ref_value) = 550000) then ticker_limit <= 65;tc<='1';
        elsif (to_integer(ref_value) = 560000) then ticker_limit <= 63;tc<='1';
        elsif (to_integer(ref_value) = 570000) then ticker_limit <= 64;tc<='1';
        elsif (to_integer(ref_value) = 580000) then ticker_limit <= 68;tc<='1';
        elsif (to_integer(ref_value) = 590000) then ticker_limit <= 64;tc<='1';
        elsif (to_integer(ref_value) = 600000) then ticker_limit <= 67;tc<='1';
        elsif (to_integer(ref_value) = 610000) then ticker_limit <= 64;tc<='1';
        elsif (to_integer(ref_value) = 620000) then ticker_limit <= 66;tc<='1';
        elsif (to_integer(ref_value) = 630000) then ticker_limit <= 64;tc<='1';
        elsif (to_integer(ref_value) = 640000) then ticker_limit <= 65;tc<='1';
        elsif (to_integer(ref_value) = 650000) then ticker_limit <= 68;tc<='1';
        elsif (to_integer(ref_value) = 660000) then ticker_limit <= 65;tc<='1';
        elsif (to_integer(ref_value) = 670000) then ticker_limit <= 67;tc<='1';
        elsif (to_integer(ref_value) = 680000) then ticker_limit <= 65;tc<='1';
        elsif (to_integer(ref_value) = 690000) then ticker_limit <= 66;tc<='1';
        elsif (to_integer(ref_value) = 700000) then ticker_limit <= 68;tc<='1';
        elsif (to_integer(ref_value) = 710000) then ticker_limit <= 66;tc<='1';
        elsif (to_integer(ref_value) = 720000) then ticker_limit <= 67;tc<='1';
        elsif (to_integer(ref_value) = 730000) then ticker_limit <= 68;tc<='1';
        elsif (to_integer(ref_value) = 740000) then ticker_limit <= 65;tc<='1';
        elsif (to_integer(ref_value) = 770000) then emit_pps <= '0'; mpps<='1';
        elsif (to_integer(ref_value) = 770400) then emit_pps <= '1'; mpps<='1';
        elsif (to_integer(ref_value) = 780000) then emit_pps <= '0'; mpps<='1';
        elsif (to_integer(ref_value) = 781000) then emit_pps <= '1'; mpps<='1';
        elsif (to_integer(ref_value) = 790000) then emit_pps <= '0'; mpps<='1';
        elsif (to_integer(ref_value) = 792000) then emit_pps <= '1'; mpps<='1';
        end if;

      else
        ticker <= ticker + 1;
      end if;
    end if;
  end process;

  process(clk)
  begin
    if (rising_edge(clk)) then
      if (i_pps = '1') then
        cur_ref_diff <= signed(o_cur_value) - signed(i_ref_value);
      end if;
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
