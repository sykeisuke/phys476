library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.ALL;
use ieee.std_logic_unsigned.ALL;
use ieee.numeric_std.all;

entity tb_timing is
  port (clk     :  in  std_logic;

        pin_in  :  in  std_logic_vector(19+48+32+32+2+1+32+9+1+9+1+4 downto 0);
        pin_out :  out std_logic_vector(18+32+32+2+1+1+4 downto 0)
        );
end tb_timing;

architecture RTL of tb_timing is

  signal ff_in : std_logic_vector(pin_in'range);
  signal ff_out: std_logic_vector(pin_out'range);

  signal in_word        : std_logic_vector (16-1 downto 0);
  signal in_got_word    : std_logic;
  signal in_new_packet  : std_logic;

  signal out_word       : std_logic_vector (16-1 downto 0);
  signal out_ena        : std_logic;
  signal out_taken      : std_logic;
  signal out_payload    : std_logic;

  signal cfg_macaddr    : std_logic_vector (47 downto 0) := (others => '0');
  signal cfg_ipaddr     : std_logic_vector (31 downto 0) := (others => '0');

  signal cfg_fixed_ip   : std_logic;
  signal cfg_dyn_ip     : std_logic;
  signal cfg_gen_rarp   : std_logic;
  signal cfg_gen_bootp  : std_logic;

  signal mdc_out        : std_logic;
  signal mdc_ena        : std_logic;
  signal mdio_in        : std_logic;
  signal mdio_out       : std_logic;
  signal mdio_ena       : std_logic;

  signal reg_addr       : std_logic_vector(24 downto 0);
  signal reg_data_wr    : std_logic_vector(31 downto 0);
  signal reg_data_rd    : std_logic_vector(31 downto 0) :=
    std_logic_vector(to_unsigned(16#1234bcde#,32));
  signal reg_write      : std_logic;
  signal reg_read       : std_logic;
  signal reg_done       : std_logic := '0';

  signal data_write     : std_logic;
  signal data_word      : std_logic_vector(31 downto 0);
  signal data_offset    : std_logic_vector( 6 downto 0);
  signal data_commit    : std_logic;
  signal data_commit_len : std_logic_vector( 6 downto 0);
  signal data_free      : std_logic;
  signal tcp_reset      : std_logic;

  signal slow_clock_tick : std_logic;
  signal timeout_tick   : std_logic;

  signal crc_d64_in     : std_logic_vector(63 downto 0);

  signal crc32          : std_logic_vector(31 downto 0);
  signal crc32_next     : std_logic_vector(31 downto 0);

  signal crc32_i1       : std_logic_vector(31 downto 0);
  signal crc32_i2       : std_logic_vector(31 downto 0);
  signal crc32_i3       : std_logic_vector(31 downto 0);

begin

  process(clk)
  begin
    if (rising_edge(clk)) then
      ff_in <= pin_in;
      pin_out <= ff_out;

    end if;
  end process;

  fnm: if (true) generate
    in_word  <= ff_in(15 downto 0);
    in_got_word <= ff_in(16);
    in_new_packet <= ff_in(17);

    out_taken <= ff_in(18);
    ff_out(15 downto 0) <= out_word;
    ff_out(16) <= out_ena;
    ff_out(17) <= out_payload;

    cfg_macaddr <= ff_in(19+47    downto 19   );
    cfg_ipaddr  <= ff_in(19+48+31 downto 19+48);

    -- ff_out (18+26 and 18+25) for the reserved future use address bits
    ff_out(18+24    downto 18   ) <= reg_addr;
    ff_out(18+32+31 downto 18+32) <= reg_data_wr;
    ff_out(18+32+32  ) <= reg_write;
    ff_out(18+32+32+1) <= reg_read;
    reg_data_rd <= ff_in(19+48+32+31 downto 19+48+32);
    reg_done <= ff_in(19+48+32+32);

    slow_clock_tick <= ff_in(19+48+32+32+1);
    timeout_tick <= ff_in(19+48+32+32+1+1);

    data_write <= ff_in(19+48+32+32+1+2);
    data_word <= ff_in(19+48+32+32+1+2+1+31 downto 19+48+32+32+1+2+1);
    data_offset <= ff_in(19+48+32+32+1+2+1+32+6 downto 19+48+32+32+1+2+1+32);
    data_commit <= ff_in(19+48+32+32+1+2+1+32+9);
    data_commit_len <= ff_in(19+48+32+32+1+2+1+32+9+1+6 downto 19+48+32+32+1+2+1+32+9+1);
    ff_out(18+32+32+2) <= data_free;
    ff_out(18+32+32+2+1) <= tcp_reset;

    mdio_in <= ff_in(19+48+32+32+2+1+32+9+1+9);
    ff_out(18+32+32+2+1+1+0) <= mdc_out;
    ff_out(18+32+32+2+1+1+1) <= mdc_ena;
    ff_out(18+32+32+2+1+1+2) <= mdio_out;
    ff_out(18+32+32+2+1+1+3) <= mdio_ena;

    cfg_fixed_ip    <= ff_in(19+48+32+32+2+1+32+9+1+9+1);
    cfg_dyn_ip      <= ff_in(19+48+32+32+2+1+32+9+1+9+1+1);
    cfg_gen_rarp    <= ff_in(19+48+32+32+2+1+32+9+1+9+1+2);
    cfg_gen_bootp   <= ff_in(19+48+32+32+2+1+32+9+1+9+1+3);

    fakernet: entity work.fakernet_module
      generic map(data_bufsize_addrbits => 13,
                  description => "x") -- Work around broken compiler.
      port map (
        ------------------------ INPUTS
        clk      => clk,
        ------------------------ CONFIG
        cfg_macaddr  => cfg_macaddr,
        cfg_ipaddr   => cfg_ipaddr,
        cfg_fixed_ip => cfg_fixed_ip,
        cfg_dyn_ip   => cfg_dyn_ip,
        cfg_gen_rarp => cfg_gen_rarp,
        cfg_gen_bootp=> cfg_gen_bootp,
        ------------------------ NETWORK INPUTS
        in_word  => in_word,
        in_got_word => in_got_word,
        in_new_packet => in_new_packet,
        ------------------------ NETWORK OUTPUTS
        out_word  => out_word,
        out_taken => out_taken,
        out_ena   => out_ena,
        out_payload => out_payload,
        ------------------------ MDIO
        mdc_out   => mdc_out,
        mdc_ena   => mdc_ena,
        mdio_in   => mdio_in,
        mdio_out  => mdio_out,
        mdio_ena  => mdio_ena,
        ------------------------ REGISTER ACCESS
        reg_addr    => reg_addr,
        reg_data_wr => reg_data_wr,
        reg_data_rd => reg_data_rd,
        reg_write   => reg_write,
        reg_read    => reg_read,
        reg_done    => reg_done,
        ------------------------ DATA INPUT
        data_write      => data_write,
        data_word       => data_word,
        data_offset     => data_offset,
        data_commit     => data_commit,
        data_commit_len => data_commit_len,
        data_free       => data_free,
        tcp_reset       => tcp_reset,
        ------------------------ TICKER
        slow_clock_tick => slow_clock_tick,
        timeout_tick    => timeout_tick
        );
  end generate;

  crc: if (false) generate

    crc32 <= ff_in(31 downto 0);

    crc_d64_in <= ff_in(32+63 downto 32+0);

    ff_out(31 downto 0) <= crc32_next;

    d16: if (false) generate
      crc32calc : entity work.fnet_crc32
        port map (
          d16 => crc_d64_in(15 downto 0),
          crc_in => crc32,
          crc_out => crc32_next
          );
    end generate;

    d32_c16: if (false) generate
      crc32calc1 : entity work.fnet_crc32
        port map (
          d16 => crc_d64_in(15 downto 0),
          crc_in => crc32,
          crc_out => crc32_i1
          );

      crc32calc2 : entity work.fnet_crc32
        port map (
          d16 => crc_d64_in(31 downto 16),
          crc_in => crc32_i1,
          crc_out => crc32_next
          );
    end generate;

    d32_c32: if (false) generate
      crc32calc : entity work.fnet_crc32_d32
        port map (
          d32 => crc_d64_in(31 downto 0),
          crc_in => crc32,
          crc_out => crc32_next
          );
    end generate;

  end generate;

end;
