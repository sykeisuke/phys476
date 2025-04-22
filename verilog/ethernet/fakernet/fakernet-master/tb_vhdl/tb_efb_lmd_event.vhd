library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.all;

use work.fnet_records.all; -- For word32_array.
use work.fnet_util_pkg.all;

entity tb_efb_lmd_event is
end entity tb_efb_lmd_event;

architecture RTL of tb_efb_lmd_event is

  -- Should simulation continue?
  signal runsim : boolean := true;

  -- Clock
  signal clk : std_logic;

  constant period : time := 10 ns;

  --
  signal gen_ts           : std_logic := '0';

  signal timestamp_ns     : unsigned(63 downto 0) := (others => '0');
  signal timestamp_err    : std_logic := '0';

  signal trig1            : std_logic := '0';
  signal trig3            : std_logic := '0';
  signal trig13_or        : std_logic := '0';

  signal subev_type_subtype : std_logic_vector(31 downto 0) :=
    i2slv16(1) & i2slv16(10);
  signal subev_procid_crate_ctrl : std_logic_vector(31 downto 0) :=
    "00000000" & "00000010" &  "00000011" & "00000100";
  signal subev_ts_id        : std_logic_vector(7 downto 0) := "00001001";

  -- Hardware interface
  signal event_word       : std_logic_vector(31 downto 0) := (others => '0');
  signal event_offset     : std_logic_vector( 4 downto 0) := (others => '0');
  signal event_write      : std_logic := '0';
  signal event_commit_len : std_logic_vector( 4 downto 0) := (others => '0');
  signal event_commit     : std_logic := '0';
  signal event_free       : std_logic;
  signal event_reset      : std_logic;

  signal flush            : std_logic;

  signal data_offset      : std_logic_vector( 9 downto 0);
  signal data_commit_len  : std_logic_vector(10 downto 0);

  signal data_word        : std_logic_vector(31 downto 0);
  signal data_write       : std_logic;
  signal data_commit      : std_logic;

  signal prng_lfsr1 : std_logic_vector(15 downto 0) := (others => '1');

begin

  gen_ts <= '1';

  process(clk)
  begin
    if (rising_edge(clk)) then
      timestamp_ns <= timestamp_ns + 1;

      prng_lfsr1(15 downto 1) <= prng_lfsr1(14 downto 0);
      prng_lfsr1(0) <= prng_lfsr1(15) xor prng_lfsr1(13);
    end if;
  end process;

  -- Just pick triggers sometimes.
  trig1 <= '1' when (prng_lfsr1(4 downto 0) = "00001") else '0';
  trig3 <= '1' when (prng_lfsr1(4 downto 0) = "00011") else '0';

  trig13_or <= trig1 or trig3;

  sim_event: entity work.efb_sim_events
    port map (
      clk              => clk,
      -- Generation options.
      gen_ts           => gen_ts,
      -- Timestamp.
      timestamp_ns     => std_logic_vector(timestamp_ns),
      timestamp_err    => timestamp_err,
      -- Triggers.
      trig1            => trig1,
      trig3            => trig3,
      trig13_or        => trig13_or,
      trig_external    => '0',
      -- Events.
      event_word       => event_word,
      event_offset     => event_offset,
      event_write      => event_write,
      event_commit_len => event_commit_len,
      event_commit     => event_commit,
      event_free       => event_free,
      event_reset      => event_reset
      );

  flush <= '0';

  lmd_event: entity work.efb_lmd_buffer_events
    port map (
      clk              => clk,

      subev_type_subtype      => subev_type_subtype,
      subev_procid_crate_ctrl => subev_procid_crate_ctrl,
      subev_ts_id             => subev_ts_id,

      event_word       => event_word,
      event_offset     => event_offset,
      event_write      => event_write,
      event_commit_len => event_commit_len,
      event_commit     => event_commit,
      event_free       => event_free,
      event_reset      => event_reset,

      flush            => flush,

      data_word        => data_word,
      data_offset      => data_offset,
      data_write       => data_write,
      data_commit      => data_commit,
      data_commit_len  => data_commit_len,
      data_free        => '1',
      data_reset       => '0'
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

  -- Write produced output network words.
  process
    variable l : line;
    variable iv : integer;
    variable committed : unsigned(31 downto 0) := (others => '0');
  begin
    if (runsim) then
      -- Hack: get into the clock cycle, to get the latched data.
      wait for 1 ns;
      if (data_write = '1') then
        write (l, String'("WORD "));
        iv := to_integer(committed + unsigned(data_offset));
        write (l, iv);
        write (l, String'(" : "));
        hwrite (l, data_word);
        -- Also print integer version as long as bit 31 or above are
        -- not set.
        if (data_word(data_word'high downto 31) =
            (data_word'high downto 31 => '0')) then
          write (l, String'(" = "));
          iv := to_integer(unsigned(data_word));
          write (l, iv);
        end if;
        writeline (output, l);
      end if;
      if (data_commit = '1') then
        write (l, String'("COMMIT "));
        iv := to_integer(unsigned(data_commit_len));
        write (l, iv);
        write (l, String'(" => "));
        committed := committed + unsigned(data_commit_len);
        iv := to_integer(committed);
        write (l, iv);
        writeline (output, l);
      end if;
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
