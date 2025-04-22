library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.all;

use work.fnet_records.all;

entity tb_fnet_async_fifo is
end entity tb_fnet_async_fifo;

architecture RTL of tb_fnet_async_fifo is

  -- Should simulation continue?
  signal runsim : boolean := true;

  constant width : integer := 8;

  -- Clock
  signal clk_a : std_logic;
  signal clk_b : std_logic;

  constant period_a : time :=  8 ns;
  constant period_b : time := 10 ns;


  signal cnt_input_a  : std_logic_vector(width-1 downto 0) := (others => '1');

  signal input_a      : std_logic_vector(width-1 downto 0) := (others => '0');
  signal in_has_a     : std_logic := '0';
  signal in_used_a    : std_logic := '0';

  signal output_b     : std_logic_vector(width-1 downto 0) := (others => '0');
  signal out_has_b    : std_logic := '0';
  signal out_many_b   : std_logic := '0';
  signal out_used_b   : std_logic := '0';

  signal expect_b     : std_logic_vector(width-1 downto 0) := (others => '1');

  signal check_b      : std_logic := '0';
  signal match_b      : std_logic := '0';
  signal good_b       : std_logic := '0';

  signal random1_a    : unsigned(15 downto 0) := (others => '0');
  signal random1_b    : unsigned(15 downto 0) := (others => '1');
  signal mul_b        : unsigned(31 downto 0) := (others => '0');

begin
  async_fifo : entity work.fnet_async_fifo
    generic map(width => width,
                many_bits => 1)
    port map (
        clk_a        => clk_a     ,
        input_a      => input_a   ,
        in_has_a     => in_has_a  ,
        in_used_a    => in_used_a ,
        clk_b        => clk_b     ,
        output_b     => output_b  ,
        out_has_b    => out_has_b ,
        out_many_b   => out_many_b,
        out_used_b   => out_used_b
        );

  -- Drive the clock
  process
  begin
    if runsim then
      clk_a <= '1';
      wait for 4 ns;
      clk_a <= '0';
      wait for 4 ns;
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

  process(clk_a)
  begin
    if (rising_edge(clk_a)) then
      cnt_input_a <=
        std_logic_vector(unsigned(cnt_input_a) + ("" & in_used_a));

      if (random1_a < 30000) then
        in_has_a <= '0';
      else
        in_has_a <= '1';
      end if;

      random1_a <= resize((random1_a * 75),16) + 74;
    end if;
  end process;

  input_a <= cnt_input_a when (in_has_a = '1') else "10100101";--"110101010";

  check_b <= out_has_b and out_used_b;

  match_b <= '1' when (expect_b = output_b) else '0';

  good_b  <= (not check_b) or match_b;

  process(clk_b)
  begin
    if (rising_edge(clk_b)) then
      if (random1_b < 30000) then
        out_used_b <= '0';
      else
        out_used_b <= '1';
      end if;

      expect_b <= std_logic_vector(unsigned(expect_b) + ("" & check_b));

      random1_b <= resize((random1_b * 75),16) + 72;
      mul_b <= (random1_b * 75);
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
