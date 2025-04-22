library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.ALL;
use ieee.std_logic_unsigned.ALL;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.all;

entity tb_fakernet is
end entity tb_fakernet;

architecture RTL of tb_fakernet is

  -- Should simulation continue?
  signal runsim : boolean := true;

  -- Clock
  signal clk : std_logic;

  constant period : time := 10 ns;

  -- Interface
  signal macaddr    : std_logic_vector (47 downto 0) := (others => '0');
  signal ipaddr     : std_logic_vector (31 downto 0) := (others => '0');

  signal mon_macaddr : std_logic_vector (47 downto 0) := (others => '0');
  signal mon_ipaddr  : std_logic_vector (31 downto 0) := (others => '0');
  signal mon_port    : std_logic_vector (15 downto 0) := (others => '0');

  signal do_rarp    : std_logic := '0';
  signal do_bootp   : std_logic := '0';
  signal do_dhcp    : std_logic := '0';
  signal do_ntpq    : std_logic := '0';
  signal do_mon     : std_logic := '0';

  signal dyn_ip          : std_logic_vector (31 downto 0) := (others => '0');
  signal dyn_ip_prev     : std_logic_vector (31 downto 0) := (others => '0');
  signal dyn_ip_set      : std_logic := '0';
  signal dyn_ip_set_prev : std_logic := '0';

  signal in_word    : std_logic_vector (16-1 downto 0) := (others => '0');
  signal got_word   : std_logic := '0';
  signal new_packet : std_logic := '0';

  signal x_in_word    : std_logic_vector (16-1 downto 0);
  signal x_got_word   : std_logic;
  signal x_new_packet : std_logic;

  signal in_byte    : std_logic_vector (8-1 downto 0);
  signal got_byte   : std_logic;

  signal x_in_byte    : std_logic_vector (8-1 downto 0);
  signal x_got_byte   : std_logic;

  signal slip_in_word    : std_logic_vector (16-1 downto 0) := (others => '0');
  signal slip_in_got_word   : std_logic := '0';
  signal slip_in_new_packet : std_logic := '0';

  signal slip_out_taken : std_logic := '0';

  signal slip_tx_data     : std_logic_vector (8-1 downto 0);
  signal slip_tx_has_data : std_logic := '0';
  signal slip_tx_taken    : std_logic := '0';

  signal out_word   : std_logic_vector (16-1 downto 0);
  signal out_taken  : std_logic;
  signal out_payload : std_logic;
  signal out_crc     : std_logic;
  signal out_payload_prev : std_logic;
  signal out_crc_prev : std_logic;

  signal mdc_out    : std_logic;
  signal mdc_ena    : std_logic;
  signal mdio_in    : std_logic;
  signal mdio_out   : std_logic;
  signal mdio_ena   : std_logic;

  signal x_out_taken  : std_logic;

  signal reg_addr    : std_logic_vector(24 downto 0);
  signal reg_data_wr : std_logic_vector(31 downto 0);
  signal reg_data_rd : std_logic_vector(31 downto 0) :=
    std_logic_vector(to_unsigned(16#1234bcde#,32));
  signal reg_write   : std_logic;
  signal reg_read    : std_logic;
  signal reg_done    : std_logic := '0';
  signal reg_cnt     : std_logic_vector(3 downto 0);

  signal data_write     : std_logic;
  signal data_word      : std_logic_vector(31 downto 0) := (others => '0');
  signal data_offset    : std_logic_vector( 7 downto 0) := (others => '0');
  signal data_commit    : std_logic;
  signal data_len       : std_logic_vector( 5 downto 0) := (others => '0');
  signal data_free      : std_logic;

  signal tcp_reset      : std_logic;

  signal ntp_leap       : std_logic_vector(1 downto 0) := "00";
  signal ntp_cur_ts     : std_logic_vector(63 downto 0) :=
    "11100110011101110001111001101100" &
    "00000000000000000000000000000000";
  signal ntp_ref_ts     : std_logic_vector(63 downto 0) :=
    "00000000000000000000000000000000" &
    "00000000000000000000010000000000";

  signal slow_clock_tick : std_logic := '0';
  signal slow_ticker : std_logic_vector(4 downto 0) := (others => '0');
  signal timeout_tick : std_logic := '0';
  signal timeout_ticker : std_logic_vector(9 downto 0) := (others => '0');

  signal fastforward_timeout_ticker : std_logic := '0';

  signal max_packet_payload : integer := 1400;

  signal make_data_lim : integer := 0;
  signal make_data_done : integer := 0;

  signal prng_lfsr1 : std_logic_vector(15 downto 0) := (others => '1');
  signal prng_lfsr2 : std_logic_vector(15 downto 0) := (others => '1');

  signal slip : std_logic := '0';

begin
  fakernet: entity work.fakernet_module
    generic map (data_bufsize_addrbits => 11)
    port map (
      ------------------------ INPUTS -------------------------
      clk           => clk,

      ------------------------ CONFIG -------------------------
      cfg_macaddr   => macaddr,
      cfg_ipaddr    => ipaddr,
      cfg_fixed_ip  => '1',
      cfg_dyn_ip    => '1',
      cfg_gen_rarp  => do_rarp,
      cfg_gen_bootp => do_bootp,
      cfg_gen_dhcp  => do_dhcp,
      cfg_gen_ntpq  => do_ntpq,
      cfg_gen_mon   => do_mon,

      mon_macaddr   => mon_macaddr,
      mon_ipaddr    => mon_ipaddr,
      mon_udp_port  => mon_port,

      ------------------------ DYNAMIC CONFIG -----------------
      dyn_ip        => dyn_ip,
      dyn_ip_set    => dyn_ip_set,

      ------------------------ INPUTS -------------------------
      in_word       => in_word,
      in_got_word   => got_word,
      in_new_packet => new_packet,
      in_slip_frame => slip,

      ------------------------ OUTPUTS ------------------------
      out_word      => out_word,
      out_ena       => open,
      out_payload   => out_payload,
      out_crc       => out_crc,
      out_taken     => out_taken,

      ------------------------ MDIO ---------------------------
      mdc_out   => mdc_out,
      mdc_ena   => mdc_ena,
      mdio_in   => mdio_in,
      mdio_out  => mdio_out,
      mdio_ena  => mdio_ena,

      ------------------------ REGISTER ACCESS ----------------
      reg_addr    => reg_addr,
      reg_data_wr => reg_data_wr,
      reg_data_rd => reg_data_rd,
      reg_write   => reg_write,
      reg_read    => reg_read,
      reg_done    => reg_done,
      reg_cnt     => reg_cnt,

      ------------------------ DATA INPUT ---------------------
      data_write      => data_write,
      data_word       => data_word,
      data_offset     => data_offset,
      data_commit     => data_commit,
      data_commit_len => data_len,
      data_free       => data_free,

      tcp_reset       => tcp_reset,

      ntp_leap        => ntp_leap,
      ntp_cur_ts      => ntp_cur_ts,
      ntp_ref_ts      => ntp_ref_ts,

      ntpq_req        => '1',
      ntpq_mac        => "010110100000000101011010000000100101101000000011",
      ntpq_ip         => "01011010000001000101101000000101",
      ntpq_tm_hi      => "01011010000001100101101000000111",

      ------------------------ TICKER -------------------------
      slow_clock_tick => slow_clock_tick,
      timeout_tick   => timeout_tick,

      ------------------------ CONTROL ------------------------
      max_packet_payload => max_packet_payload

      );

  parse_slip: entity work.efnet_slip_rx
    port map (
      clk           => clk,
      i_data        => in_byte,
      i_has_data    => got_byte,

      o_word_1      => slip_in_word(15 downto  8),
      o_word_2      => slip_in_word( 7 downto  0),
      o_words_ready => slip_in_got_word,
      o_packet_start => slip_in_new_packet
      );

  send_slip: entity work.efnet_slip_tx
    port map (
      clk           => clk,

      out_word      => out_word,
      out_payload   => out_payload,
      out_crc       => out_crc,
      out_taken     => slip_out_taken,

      o_data        => slip_tx_data,
      o_has_data    => slip_tx_has_data,
      i_taken       => slip_tx_taken
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

  -- Read input file with data to compress
  testprocess: process is
    variable row    : line;
    variable rowval : std_logic_vector(16-1 downto 0);
    variable gotval : boolean;
    variable read_data : std_logic_vector (16-1 downto 0);
    variable l      : line;
    variable gmacaddr : std_logic_vector(47 downto 0);
    variable gipaddr : std_logic_vector(31 downto 0);
    variable gport  : std_logic_vector(15 downto 0);
    variable gdodyn : std_logic_vector(0 downto 0);
    variable gdontpq : std_logic_vector(0 downto 0);
    variable gdomon : std_logic_vector(0 downto 0);
    variable intval : integer := 0;
  begin
    x_new_packet <= '0';
    x_got_word <= '0';
    x_in_word <= std_logic_vector(to_unsigned(0, in_word'length));
    -- Hack: get a bit into the clock cycle, to set the data to hold.
    wait for 1 ns;
    -- Wait a few clock cycles to get rid of meta values.
    wait for 100 ns;

    wait for 30 ns;
    --x_got_word <= '0';
    --~ wait for period;

    while (not endfile(input)) loop
      readline(input,row);
      x_got_word <= '0';
      x_new_packet <= '0';
      if (row'length >= 1 and row.all(1) = '#') then
        null; -- Comment - do nothing.
      elsif (slip = '0' and (row'length = 4 or
                             (row'length > 4 and row.all(5) = ' '))) then
        while (row'length >= 4) loop
          hread(row,rowval,gotval);
          if (not gotval) then
            exit;
          end if;
          read_data := rowval;--std_logic_vector(to_unsigned(rowval,16));
          x_in_word <= read_data(x_in_word'range);
          x_got_word <= '1';
          wait for 1 ns;
          --
          write (l, String'("INPUT: 0x"));
          hwrite (l, x_in_word);
          write (l, String'(" = "));
          write (l, x_in_word);
          writeline (output, l);
          --
          wait for 9 ns;
          -- Original (non-randomised) had 10 ns delay.
          -- This gives (1-2^-10)*10 ns = 9.99 ns.
          -- Note: as soon as one iteration gives no delay, all remaining
          -- also give no delay.
          for i in 1 to 10 loop
            if (prng_lfsr1(0) = '1') then
              x_got_word <= '0';
              wait for 10 ns;
            end if;
          end loop;
        end loop;
      elsif (slip = '1' and (row'length = 2 or
                             (row'length > 2 and row.all(3) = ' '))) then
        while (row'length >= 2) loop
          hread(row,rowval(7 downto 0),gotval);
          if (not gotval) then
            exit;
          end if;
          read_data := rowval;
          x_in_byte <= read_data(x_in_byte'range);
          x_got_byte <= '1';
          wait for 1 ns;
          --
          write (l, String'("UART-INPUT: 0x"));
          hwrite (l, x_in_byte);
          write (l, String'(" = "));
          write (l, x_in_byte);
          writeline (output, l);
          --
          wait for 9 ns;
          x_got_byte <= '0';
          -- Original (non-randomised) had 10 ns delay.
          -- This gives (1-2^-10)*10 ns = 9.99 ns.
          -- Note: as soon as one iteration gives no delay, all remaining
          -- also give no delay.
          for i in 1 to 10 loop
            if (prng_lfsr1(0) = '1') then
              wait for 10 ns;
            end if;
          end loop;
          -- UART is slow, always have cycles between input words.
          -- In fact, there are at least 10 bit slots (the 10 bits),
          -- and each bit-slot has at least two cycles.
          wait for 190 ns;
        end loop;
      else
        --write (l, row(row'left));
        --writeline (output, l);
        if (row(row'left to row'right) = String'("SetMAC")) then
          readline(input,row);
          hread(row,gmacaddr,gotval);
          macaddr <= gmacaddr;
          wait for 10 ns; -- let assignment take effect
          write (l, String'("MACADDR "));
          hwrite (l, macaddr);
          writeline (output, l);
        elsif (row(row'left to row'right) = String'("SetIP")) then
          readline(input,row);
          hread(row,gipaddr,gotval);
          ipaddr <= gipaddr;
          wait for 10 ns; -- let assignment take effect
          write (l, String'("IPADDR "));
          hwrite (l, ipaddr);
          writeline (output, l);
        elsif (row(row'left to row'right) = String'("SetMONMAC")) then
          readline(input,row);
          hread(row,gmacaddr,gotval);
          mon_macaddr <= gmacaddr;
          wait for 10 ns; -- let assignment take effect
          write (l, String'("MONMACADDR "));
          hwrite (l, mon_macaddr);
          writeline (output, l);
        elsif (row(row'left to row'right) = String'("SetMONIP")) then
          readline(input,row);
          hread(row,gipaddr,gotval);
          mon_ipaddr <= gipaddr;
          wait for 10 ns; -- let assignment take effect
          write (l, String'("MONIPADDR "));
          hwrite (l, mon_ipaddr);
          writeline (output, l);
        elsif (row(row'left to row'right) = String'("SetMONPort")) then
          readline(input,row);
          hread(row,gport,gotval);
          mon_port <= gport;
          wait for 10 ns; -- let assignment take effect
          write (l, String'("MONPORT "));
          hwrite (l, mon_port);
          writeline (output, l);
        elsif (row(row'left to row'right) = String'("DoRARP")) then
          readline(input,row);
          read(row,gdodyn,gotval);
          do_rarp <= gdodyn(0);
          wait for 10 ns; -- let assignment take effect
          write (l, String'("DORARP "));
          write (l, do_rarp);
          writeline (output, l);
        elsif (row(row'left to row'right) = String'("DoBOOTP")) then
          readline(input,row);
          read(row,gdodyn,gotval);
          do_bootp <= gdodyn(0);
          wait for 10 ns; -- let assignment take effect
          write (l, String'("DOBOOTP "));
          write (l, do_bootp);
          writeline (output, l);
        elsif (row(row'left to row'right) = String'("DoDHCP")) then
          readline(input,row);
          read(row,gdodyn,gotval);
          do_dhcp <= gdodyn(0);
          wait for 10 ns; -- let assignment take effect
          write (l, String'("DODHCP "));
          write (l, do_dhcp);
          writeline (output, l);
        elsif (row(row'left to row'right) = String'("DoNTPQ")) then
          readline(input,row);
          read(row,gdontpq,gotval);
          do_ntpq <= gdontpq(0);
          wait for 10 ns; -- let assignment take effect
          write (l, String'("DONTPQ "));
          write (l, do_ntpq);
          writeline (output, l);
        elsif (row(row'left to row'right) = String'("DoMON")) then
          readline(input,row);
          read(row,gdomon,gotval);
          do_mon <= gdomon(0);
          wait for 10 ns; -- let assignment take effect
          write (l, String'("DOMON "));
          write (l, do_mon);
          writeline (output, l);
        elsif (row(row'left to row'right) = String'("ReadSLIP")) then
          write (l, String'("READSLIP"));
          writeline (output, l);
          slip <= '1';
        elsif (row(row'left to row'right) = String'("Packet")) then
          write (l, String'("NEW-PACKET"));
          writeline (output, l);
          x_new_packet <= '1';
          x_got_word <= '1';
          -- Not really, but a dummy
          x_in_word <= "1111000011110000";
        elsif (row(row'left to row'right) = String'("FinPacket")) then
          write (l, String'("FIN-PACKET"));
          writeline (output, l);
          --x_got_word <= '1';
          -- Not really, but a dummy
          x_in_word <= "1111000011110000";
        elsif (row(row'left to row'right) = String'("Sleep")) then
          write (l, String'("SLEEP"));
          writeline (output, l);
          -- Not really, but a dummy
          x_in_word <= "0001000000000001";
          wait for 1500 ns;
        elsif (row(row'left to row'right) = String'("MaxPayload")) then
          readline(input,row);
          read(row,intval);
          max_packet_payload <= intval;
          wait for 10 ns; -- let assignment take effect
          write (l, String'("MAX-PAYLOAD "));
          write (l, max_packet_payload);
          writeline (output, l);
          x_in_word <= "1111000011110000";
        elsif (row(row'left to row'right) = String'("MakeData")) then
          readline(input,row);
          read(row,intval);
          make_data_lim <= make_data_lim + intval;
          wait for 10 ns; -- let assignment take effect
          write (l, String'("MAKE-DATA "));
          write (l, intval);
          write (l, String'(" LIM "));
          write (l, make_data_lim);
          writeline (output, l);
          x_in_word <= "1111000011110000";
        elsif (row(row'left to row'right) = String'("TimeoutTicks")) then
          readline(input,row);
          read(row,intval);
          write (l, String'("TIMEOUT-TICKS "));
          write (l, intval);
          writeline (output, l);
          fastforward_timeout_ticker <= '1';
          wait for intval*3*10 ns;
          fastforward_timeout_ticker <= '0';
        elsif (row(row'left to row'right) = String'("Empty") or
               row'length = 0) then
          write (l, String'("IN-EMPTY"));
          writeline (output, l);
          --x_got_word <= '1';
          -- Not really, but a dummy
          x_in_word <= "1111001011110010";
          -- Some additional cycles...
          wait for 30 ns;
          x_got_word <= '0';
          wait for 30 ns;
        else
          assert (FALSE) report "Unhandled input: '" & row.all & "'."
            severity failure;
        end if;
        wait for 10 ns;
      end if;
    end loop;

    -- We are done providing data values
    x_got_word <= '0';
    x_new_packet <= '0';
    -- We wait a little until we terminate.
    wait for 100 ns;
    write (l, String'("END"));
    writeline (output, l);

    -- Terminate simulation.
    runsim <= false;
    wait;

    assert (FALSE) report "Simulation end." severity failure;

  end process testprocess;

  out_taken  <= x_out_taken when (slip = '0') else slip_out_taken;

  process(clk)
  begin
    if (rising_edge(clk)) then
      if (slip = '0') then
        in_word    <= x_in_word;
        got_word   <= x_got_word;
        new_packet <= x_new_packet;
      else
        in_word    <= slip_in_word;
        got_word   <= slip_in_got_word;
        new_packet <= slip_in_new_packet;
      end if;

      in_byte    <= x_in_byte;
      got_byte   <= x_got_byte;

      prng_lfsr1(15 downto 1) <= prng_lfsr1(14 downto 0);
      prng_lfsr1(0) <= prng_lfsr1(15) xor prng_lfsr1(13);

      prng_lfsr2(15 downto 1) <= prng_lfsr2(14 downto 0);
      prng_lfsr2(0) <= prng_lfsr2(15) xor prng_lfsr2(11);
    end if;
  end process;

  process(clk)
  begin
    if (rising_edge(clk)) then
      data_write <= '0';
      data_commit <= '0';
      data_offset <= (others => '0');
      data_len <= (0 => '1', others => '0'); -- 1
      if (data_free = '1' and
          make_data_done < make_data_lim) then
        data_write <= '1';
        data_word <= data_word + 4;
        -- Same bits in 23..18 as 7..2
	data_word(23 downto 18) <= data_word(7 downto 2) + 1;
        data_word(31 downto 24) <= "1010" & "0101";
        data_commit <= '1';
        make_data_done <= make_data_done + 4;
      end if;
      if (tcp_reset = '1') then
        data_word <= (others => '0');
      end if;
    end if;
  end process;

  -- Write produced output network words.
  process
    variable l : line;
  begin
    if runsim then
      -- Hack: get into the clock cycle, to get the latched data.
      wait for 1 ns;
      if (slip = '0') then
        if (true) then -- TODO: random/sometimes if (out_word = '1') then
          if (out_payload = '1') then
            write (l, String'("OUTPUT: 0x"));
            hwrite (l, out_word);
            write (l, String'(" = "));
            write (l, out_word);
            writeline (output, l);
          end if;
          x_out_taken <= '1';

          if (out_payload      = '0' and
              out_payload_prev = '1') then
            write (l, String'("END-PACKET"));
            writeline (output, l);
          end if;
          out_payload_prev <= out_payload;

          if (dyn_ip     /= dyn_ip_prev or
              dyn_ip_set /= dyn_ip_set_prev) then
            write (l, String'("NEW-DYN-IP "));
            write (l, dyn_ip_set);
            write (l, String'(" ; "));
            hwrite (l, dyn_ip);
            writeline (output, l);
          end if;
          dyn_ip_prev     <= dyn_ip;
          dyn_ip_set_prev <= dyn_ip_set;
        else
          x_out_taken <= '0';
        end if;
        -- Original (non-randomised) had 10 ns delay.
        -- This gives (1-2^-5)*10 ns = 9.6 ns.
        -- Note: as soon as one iteration gives no delay, all remaining
        -- also give no delay.
        for i in 1 to 5 loop
          if (prng_lfsr2(0) = '1') then
            wait for 10 ns;
            x_out_taken <= '0';
          end if;
        end loop;
      end if;
      wait for 9 ns;
    else
      wait;
    end if;
  end process;

  -- Write data parsed by slip reader
  process
    variable l : line;
  begin
    if runsim then
      wait for 1 ns;

      if (slip_in_got_word = '1') then
        write (l, String'("SLIP-INPUT: 0x"));
        hwrite (l, slip_in_word);
        write (l, String'(" = "));
        write (l, slip_in_word);
        writeline (output, l);
      end if;

      if (slip_in_new_packet = '1') then
        write (l, String'("SLIP-NEW-PACKET"));
        writeline (output, l);
      end if;

      if (slip = '1') then
        if (out_payload = '1' and
            slip_out_taken = '1') then
          write (l, String'("SLIP-OUTPUT: 0x"));
          hwrite (l, out_word);
          write (l, String'(" = "));
          write (l, out_word);
          writeline (output, l);

          if (out_crc = '1' and out_crc_prev = '1') then
            write (l, String'("SLIP-END-PACKET"));
            writeline (output, l);
          end if;
          out_crc_prev <= out_crc;
        end if;

        if (slip_tx_has_data = '1') then
          write (l, String'("UART-OUTPUT: "));
          hwrite (l, slip_tx_data);
          write (l, String'(" = "));
          write (l, slip_tx_data);
          writeline (output, l);
          slip_tx_taken <= '1';
        else
          slip_tx_taken <= '0';
        end if;
      end if;

      wait for 9 ns;
    else
      wait;
    end if;
  end process;

  process(clk)
  begin
    if (rising_edge(clk)) then
      slow_ticker <= slow_ticker + 1;
      slow_clock_tick <= '0';
      if (slow_ticker = 0) then
        slow_clock_tick <= '1';
      end if;

      timeout_ticker <= timeout_ticker + 1;
      if (fastforward_timeout_ticker = '1' and timeout_ticker > 2) then
        timeout_ticker <= (others => '0');
      end if;
      timeout_tick <= '0';
      if (timeout_ticker = 0) then
        timeout_tick <= '1';
      end if;
    end if;
  end process;

  process (clk)
  begin
    if (rising_edge(clk)) then
      ntp_cur_ts(31 downto 0) <= ntp_cur_ts(31 downto 0) + 43;
    end if;
  end process;

end architecture RTL;
