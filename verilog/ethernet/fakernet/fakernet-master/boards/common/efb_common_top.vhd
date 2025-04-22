-- Copyright (c) 2020, Anders Furufors
-- Copyright (c) 2020, Haakan T. Johansson
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the authors nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

use work.fnet_records.all; -- For word32_array.
use work.fnet_util_pkg.all; -- For fnet_or_reduction.
use work.text_config_data.all; -- For text config option indices.

entity efb_common_top is
  generic (compiletime : integer := 1;
           description : string := "efb_common_top"; -- Just replace value!
           dynamic_gen : integer := 1;
           num_pmod_gps : integer := 0;
           pmod_gps_pps_nsubsampl : integer := 1;
           clk_freq    : integer);
  port (
    -- On-board clock.
    clk            : in  std_logic;
    clk25          : in  std_logic;

    spi_cfg_base_addr : in std_logic_vector(23 downto 0);

    cfg_ipaddr     : in  std_logic_vector(31 downto 0);

    -- Ethernet PHY:
    -- Control channel.
    eth_mdc        : out std_logic := '0';
    eth_mdio       : inout std_logic := '0';
    eth_rstn       : out std_logic := '1';
    -- TX channel.
    -- eth_txd        : out std_logic_vector(3 downto 0) := (others => '0');
    -- eth_tx_en      : out std_logic := '0';
    -- eth_tx_clk     : in  std_logic;
    out_word       : out std_logic_vector(15 downto 0);
    out_ena        : out std_logic;
    out_payload    : out std_logic;
    out_taken      : in  std_logic;
    -- RX channel.
    -- eth_rxd        : in  std_logic_vector(3 downto 0);
    -- eth_rx_clk     : in  std_logic;
    -- eth_rx_dv      : in  std_logic;
    -- eth_rxerr      : in  std_logic;
    in_word        : in  std_logic_vector(15 downto 0);
    in_got_word    : in  std_logic;
    in_new_packet  : in  std_logic;
    -- Link status.
    eth_col        : in  std_logic;
    eth_crs        : in  std_logic;
    -- Ref clk.
    eth_ref_clk    : out std_logic;

    -- XADC
    xadc_data_request : out std_logic := '0';
    xadc_data_array    : in  word32_array(0 downto 0) :=
      (others => (others => '0'));
    xadc_has_data      : in  std_logic := '0';
    xadc_data_pending  : out std_logic := '0';

    -- SPI interface.
    spi_sdi        : in    std_logic;
    spi_csn        : inout std_logic := '1';
    spi_sdo        : inout std_logic := '1';
    spi_wpn        : out   std_logic := '1';
    spi_hldn       : out   std_logic := '1';
    spi_sck        : inout std_logic := '1';

    -- User input.
    sw             : in  std_logic_vector(3 downto 0) := (others => '0');
    btn            : in  std_logic_vector(3 downto 0) := (others => '0');

    -- LEDs.
    led            : out std_logic_vector(3 downto 0);
    led_r          : out std_logic_vector(3 downto 0);
    led_g          : out std_logic_vector(3 downto 0);
    led_b          : out std_logic_vector(3 downto 0);

    -- PMOD

    -- PMOD GPS
    -- Note: RX/TX swapped w.r.t. module names, here seen from the FPGA.
    pmod_gps_3dfix : in  std_logic_vector(0 to num_pmod_gps-1); -- From GPS.
    pmod_gps_tx    : out std_logic_vector(0 to num_pmod_gps-1); -- To GPS.
    pmod_gps_rx    : in  std_logic_vector(0 to num_pmod_gps-1); -- From GPS.
    pmod_gps_pps_samples : in -- 0 is most recent sample        -- From GPS.
      std_logic_vector(num_pmod_gps * (pmod_gps_pps_nsubsampl+2) - 1 downto
                       0);

    -- UART
    uart_rx        : in  std_logic;
    uart_tx        : out std_logic;

    -- Clock count
    o_cycle_count  : out unsigned(31 downto 0);

    -- Rataclock timestamps.
    rataclock_send : out std_logic;
    rataclock_recv : in  std_logic := '0';

    trig1_send     : out std_logic;
    trig3_send     : out std_logic;

    trig1_recv     : in  std_logic := '0';
    trig3_recv     : in  std_logic := '0';

    -- Sampler data stream.
    sampler_data_array   : in  word32_array;
    sampler_has_data     : in  std_logic_vector;
    sampler_data_pending : out std_logic_vector;
    -- Sampler pulser control.
    sampler_pulse_control : out std_logic_vector(31 downto 0)

    );

end efb_common_top;

architecture RTL of efb_common_top is

  -- FPGA pseudo-unique ID (may be same on 32 chips).
  signal fpga_dna           : std_logic_vector(57-1 downto 0);

  -------------------------
  -- PHY control signals --
  -------------------------

  signal phy_reset_counter : unsigned(22 downto 0) := (others => '0');

  --------------------------------
  -- Input word builder signals --
  --------------------------------

  signal packet_counter  : std_logic_vector(3 downto 0) := (others => '0');

  ----------------------
  -- Fakernet signals --
  ----------------------

  signal cfg_fixed_ip   : std_logic := '1';
  signal cfg_dyn_ip     : std_logic := '0';
  signal cfg_gen_rarp   : std_logic := '0';
  signal cfg_gen_bootp  : std_logic := '0';
  signal cfg_gen_dhcp   : std_logic := '0';
  signal cfg_gen_ntpq   : std_logic := '0';

  signal cfg_udp_mdio     : std_logic := '0';
  signal cfg_udp_testctrl : std_logic := '0';

  -- MAC and IP set manually
  -- 02:00:12:34:20:00
  -- MAC note: low bits of high octet (bits 41..40) "10" is
  -- 'locally administered' and 'unicast'
  signal macaddr : std_logic_vector(47 downto 0) :=
    "00000010" & "00000000" &
    "00010010" & "00110100" & "00100000" & "00000000";

  -- Values from text config.
  signal tcfg_macaddr : std_logic_vector(47 downto 0) := (others => '0');
  signal tcfg_ipaddr  : std_logic_vector(31 downto 0) := (others => '0');

  -- Text config values given.
  signal tcfg_has_macaddr : std_logic := '0';
  signal tcfg_has_ipaddr  : std_logic := '0';

  -- Actual values to use.
  signal sel_macaddr : std_logic_vector(47 downto 0) := (others => '0');
  signal sel_ipaddr  : std_logic_vector(31 downto 0) := (others => '0');

  -- Data from Fakernet to PHY?
  signal debug_state_in  : std_logic_vector(7 downto 0);
  signal debug_state_out : std_logic_vector(3 downto 0);

  signal in_info  : incoming_info_counts;
  signal out_info : outgoing_info_counts;

  -- Data buffer signals.
  signal data_word       : std_logic_vector(31 downto 0);
  signal data_offset     : std_logic_vector(9 downto 0);
  signal data_write      : std_logic;
  signal data_commit_len : std_logic_vector(10 downto 0);
  signal data_commit     : std_logic;
  signal data_free       : std_logic;
  signal data_reset       : std_logic;

  -- Slow tick counter every 2^7 = 128 clock cycles = 1.28 us.
  signal slow_counter : std_logic_vector(6 downto 0) := (others => '0');
  signal slow_counter_tick : std_logic := '0';

  -- Timeout tick every 2^26 = 67.1*10^6 clock cycles = 671 ms.
  signal timeout_counter : std_logic_vector(26 downto 0) := (others => '0');
  signal timeout_counter_tick : std_logic := '0';

  -- Register access interface
  signal regacc_addr    : std_logic_vector(24 downto 0);
  signal regacc_data_wr : std_logic_vector(31 downto 0);
  signal regacc_data_rd : std_logic_vector(31 downto 0);
  signal regacc_write   : std_logic;
  signal regacc_read    : std_logic;
  signal regacc_done    : std_logic;
  signal regacc_cnt     : std_logic_vector(3 downto 0);

  -- MDIO interface
  signal mdc_out    : std_logic;
  signal mdc_ena    : std_logic;
  signal mdio_in    : std_logic := '0';
  signal mdio_out   : std_logic;
  signal mdio_ena   : std_logic;

  -----------------------------------------
  -- User button, switch and LED signals --
  -----------------------------------------

  signal sw_latch       : std_logic_vector(3 downto 0);
  signal btn_latch      : std_logic_vector(3 downto 0);

  signal rgb_led_stat_r : std_logic_vector(3 downto 0) := (others => '0');
  signal rgb_led_stat_g : std_logic_vector(3 downto 0) := (others => '0');
  signal rgb_led_stat_b : std_logic_vector(3 downto 0) := (others => '0');
  signal rgb_led_pkt_info : std_logic := '1';

  signal rgb_led_info1 : std_logic_vector(11 downto 0) := (others => '0');
  signal rgb_led_info2 : std_logic_vector(11 downto 0) := (others => '0');
  signal rgb_led_info3 : std_logic_vector(11 downto 0) := (others => '0');
  signal rgb_led_stretch : std_logic_vector(11 downto 0) := (others => '0');

  signal rgb_counter    : std_logic_vector(15 downto 0) := (others => '0');
  signal rgb_frac       : std_logic_vector(15 downto 0) :=
    (10 => '1', others => '0'); -- 0x0400

  -- Pipeline the LED outputs to allow potential routing.
  -- Also allows an anti-metastable if we directly source an input.
  signal led_ppl        : std_logic_vector(3 downto 0);
  signal led_r_ppl      : std_logic_vector(3 downto 0);
  signal led_g_ppl      : std_logic_vector(3 downto 0);
  signal led_b_ppl      : std_logic_vector(3 downto 0);

  -----------------------------
  -- UART packet info counts --
  -----------------------------

  signal infoc_out_udp : std_logic := '0';

  signal infoc_pending : std_logic_vector(63 downto 0) := (others => '0');
  signal infoc_cycle   : unsigned(5 downto 0) := (others => '0');

  signal infoc_tx_data     : std_logic_vector(7 downto 0) := (others => '0');
  signal infoc_tx_has_data : std_logic := '0';

  ----------------
  -- UART trace --
  ----------------

  signal trace_signals  : std_logic_vector(31 downto 0) := (others => '0');
  signal trace_insert   : std_logic := '0';
  signal trace_trigger  : std_logic := '0';

  signal trace_tx_data     : std_logic_vector(7 downto 0) := (others => '0');
  signal trace_tx_has_data : std_logic := '0';

  ------------------
  -- UART control --
  ------------------

  signal uart_tx_data      : std_logic_vector(7 downto 0) := (others => '0');
  signal uart_tx_has_data  : std_logic := '0';
  signal uart_tx_taken     : std_logic := '0';
  signal uart_tx_temp      : std_logic := '0';

  signal uart_rx_data      : std_logic_vector(7 downto 0) := (others => '0');
  signal uart_rx_has_data  : std_logic := '0';
  signal uart_rx_temp      : std_logic := '0';

  -- Debugging via data stream out.
  constant num_uart_data : integer := 1;

  signal uart_mon_data_array   : word32_array(0 to num_uart_data-1) :=
    (others => (others => '0'));
  signal uart_mon_has_data     : std_logic_vector(0 to num_uart_data-1) :=
    (others => '0');
  signal uart_mon_data_pending : std_logic_vector(0 to num_uart_data-1) :=
    (others => '0');

  ---------------
  -- UART SLIP --
  ---------------

  -- Input network traffic
  signal slip_in_word       : std_logic_vector(15 downto 0) := (others => '0');
  signal slip_in_got_word   : std_logic := '0';
  signal slip_in_new_packet : std_logic := '0';

  -- Output network traffic
  signal slip_out_word      : std_logic_vector(15 downto 0) :=
    std_logic_vector(to_unsigned(16#dead#,16));
  signal slip_out_ena       : std_logic := '0';
  signal slip_out_payload   : std_logic := '0';
  signal slip_out_crc       : std_logic := '0';
  signal slip_out_taken     : std_logic := '0';

  signal slip_tx_data      : std_logic_vector(7 downto 0) := (others => '0');
  signal slip_tx_has_data  : std_logic := '0';

  --------------
  -- PMOD GPS --
  --------------

  signal cycle_count      : unsigned(31 downto 0) := (others => '0');
  signal cycle_count_next : unsigned(31 downto 0) := (others => '0');
  signal cycle_count_wrap : unsigned(31 downto 0) := (others => '0');

  signal gps_pps_pulse  : std_logic_vector(0 to num_pmod_gps-1) :=
    (others => '0');

  type word2_array is
    array (integer range <>) of std_logic_vector(1 downto 0);
  type word8_array is
    array (integer range <>) of std_logic_vector(7 downto 0);

  signal ntp_leap       : word2_array(0 to num_pmod_gps-1) := (others => "11");
  signal ntp_prec       : word8_array(0 to num_pmod_gps-1) :=
    (others => (others => '1'));
  signal ntp_root_disp  : word32_array(0 to num_pmod_gps-1) :=
    (others => (others => '1'));
  signal ntp_cur_ts     : word64_array(0 to num_pmod_gps-1) :=
    (others => (others => '0'));
  signal ntp_ref_ts     : word64_array(0 to num_pmod_gps-1) :=
    (others => (others => '0'));

  signal ntp_leap_0     : std_logic_vector(1 downto 0) := "11";
  signal ntp_prec_0     : std_logic_vector(7 downto 0) := (others => '1');
  signal ntp_root_disp_0: std_logic_vector(31 downto 0) := (others => '1');
  signal ntp_cur_ts_0   : std_logic_vector(63 downto 0) := (others => '0');
  signal ntp_ref_ts_0   : std_logic_vector(63 downto 0) := (others => '0');

  constant num_gps_mon_data : integer := 22+num_pmod_gps*14;

  signal gps_mon_data_array   : word32_array(0 to num_gps_mon_data-1) :=
    (others => (others => '0'));
  signal gps_mon_has_data     : std_logic_vector(0 to num_gps_mon_data-1) :=
    (others => '0');
  signal gps_mon_data_pending : std_logic_vector(0 to num_gps_mon_data-1) :=
    (others => '0');

  signal gps_uart_tx_data : std_logic_vector(7 downto 0) := (others => '0');
  signal gps_uart_tx_hasdata : std_logic := '0';
  signal gps_uart_tx_taken   : std_logic := '0';
  signal gps_uart_tx_temp    : std_logic := '0';
  signal gps_uart_tx         : std_logic := '0';

  ----------------------------------------
  -- NTP query generation and recording --
  ----------------------------------------

  signal ntpq_req : std_logic := '0';
  signal ntpq_mac : std_logic_vector(47 downto 0) := (others => '0');
  signal ntpq_ip  : std_logic_vector(31 downto 0) := (others => '0');
  signal ntpq_tm_hi : std_logic_vector(31 downto 0) := (others => '0');
  signal ntpq_tm_lo : std_logic_vector(31 downto 0) := (others => '0');
  signal ntpq_sent : std_logic := '0';

  signal ntpr_got     : std_logic;
  signal ntpr_ip      : std_logic_vector(31 downto 0);
  signal ntpr_recv_ts : std_logic_vector(63 downto 0);
  signal ntpr_data    : word32_array(0 to 11);

  ----------
  -- XADC --
  ----------

  constant num_xadc_data : integer := 1;

  --------------------
  -- SPI flash read --
  --------------------

  signal spir_addr       : std_logic_vector(23 downto 0) := (others => '0');
  signal spir_start      : std_logic := '0';

  signal spir_data       : std_logic_vector(7 downto 0);
  signal spir_has_data   : std_logic;

  signal spir_p1_csn     : std_logic;
  signal spir_p1_sdo     : std_logic;
  signal spir_p1_sdi     : std_logic;
  signal spir_p1_sck     : std_logic;

  signal spir_p2_csn     : std_logic;
  signal spir_p2_sdo     : std_logic;
  signal spir_p2_sdi     : std_logic;
  signal spir_p2_sck     : std_logic;

  signal spir_cnt_csn    : unsigned(7 downto 0) := (others => '0');
  signal spir_cnt_sdo    : unsigned(7 downto 0) := (others => '0');
  signal spir_cnt_sdi    : unsigned(7 downto 0) := (others => '0');
  signal spir_cnt_sck    : unsigned(7 downto 0) := (others => '0');

  -- By config parser.
  signal spir_cfg_addr     : std_logic_vector(23 downto 0) := (others => '0');
  signal spir_cfg_start    : std_logic := '0';

  signal spir_cfg_data     : std_logic_vector(7 downto 0);
  signal spir_cfg_has_data : std_logic;

  signal spir_cfg_data_last     : std_logic_vector(7 downto 0);
  signal spir_cfg_has_data_last : std_logic;

  -- By UDP register access (for debug).
  signal spir_reg_addr     : std_logic_vector(23 downto 0) := (others => '0');
  signal spir_reg_start    : std_logic := '0';

  signal spir_reg_data     : std_logic_vector(7 downto 0);
  signal spir_reg_has_data : std_logic;

  signal spir_reg_has_data_latched : std_logic := '0';

  -- Text config output value (one at a time).
  signal cfg_value       : std_logic_vector(47 downto 0) := (others => '0');
  signal cfg_value_index : std_logic_vector(24 downto 0) := (others => '0');
  signal cfg_has_value   : std_logic := '0';
  signal cfg_value_done  : std_logic := '0';
  signal cfg_value_used  : std_logic := '0';
  -- Text config overall completion status.
  signal cfg_done        : std_logic := '0';
  signal cfg_fail        : std_logic := '0';
  signal cfg_fail_offset : std_logic_vector( 9 downto 0) := (others => '0');
  signal cfg_fail_code   : std_logic_vector( 3 downto 0) := (others => '0');
  -- Debug.
  signal cfg_dbg_has_data_pend : std_logic := '0';
  signal cfg_dbg_reset_parse   : std_logic := '0';
  signal cfg_dbg_active        : std_logic := '0';
  signal cfg_dbg_state_code    : std_logic_vector( 4 downto 0) :=(others=>'0');

  -- Text config output value (one at a time).
  signal cfg_last_value       : std_logic_vector(47 downto 0) := (others=>'0');
  signal cfg_last_value_index : std_logic_vector(24 downto 0) := (others=>'0');
  signal cfg_last_has_value   : std_logic := '0';

  ---------------------------------------
  -- Streaming data output (simulator) --
  ---------------------------------------

  -- Data buffer signals.
  signal sim_data_word       : std_logic_vector(31 downto 0);
  signal sim_data_offset     : std_logic_vector(9 downto 0);
  signal sim_data_write      : std_logic;
  signal sim_data_commit_len : std_logic_vector(10 downto 0);
  signal sim_data_commit     : std_logic;
  signal sim_data_free       : std_logic;
  signal sim_data_reset      : std_logic;

  -- Choose data source.  ("00" = monitor, "01" = sim, "10" = LMD)
  signal data_source : std_logic_vector(1 downto 0) := "00";

  -------------------------------------
  -- Streaming data output (monitor) --
  -------------------------------------

  constant num_sampler_data : integer := sampler_data_array'length;

  constant off_sampler_data : integer := 0;
  constant off_gps_mon_data : integer := off_sampler_data + num_sampler_data;
  constant off_xadc_data    : integer := off_gps_mon_data + num_gps_mon_data;
  constant off_uart_data    : integer := off_xadc_data + num_xadc_data;
  constant num_mon_data     : integer := off_uart_data + num_uart_data;

  signal mon_data_array   : word32_array(0 to num_mon_data-1) :=
    (others => (others => '0'));
  signal mon_has_data     : std_logic_vector(0 to num_mon_data-1) :=
    (others => '0');
  signal mon_data_pending : std_logic_vector(0 to num_mon_data-1) :=
    (others => '0');

  -- Data buffer signals.
  signal mon_data_word       : std_logic_vector(31 downto 0);
  signal mon_data_offset     : std_logic_vector(9 downto 0);
  signal mon_data_write      : std_logic;
  signal mon_data_commit_len : std_logic_vector(10 downto 0);
  signal mon_data_commit     : std_logic;
  signal mon_data_free       : std_logic;
  signal mon_data_reset      : std_logic;

  signal sampler_pulse_control_int : std_logic_vector(31 downto 0) :=
    (others => '0');

  ---------------------------
  -- Rataclock timestamps. --
  ---------------------------

  -- Timestamp (free-running).
  signal timestamp_ns  : std_logic_vector(63 downto  0) := (others => '0');

  -- Rataclock sender.
  signal rc_send_aux_sigs : std_logic_vector(4 downto 0) := (others => '0');
  signal rc_send_info_bit : std_logic := '0';

  signal rc_send_serial      : std_logic := '0';
  signal rc_send_serial_sync : std_logic := '0';

  -- Rataclock receiver.
  signal rc_recv_serial     : std_logic := '0';

  signal rc_recv_tick_ns_lo : std_logic_vector(31 downto  0) := (others => '0');
  signal rc_recv_tick_ns_hi : std_logic_vector(63 downto 32) := (others => '0');
  signal rc_recv_aux_sigs   : std_logic_vector(4 downto 0) := (others => '0');
  signal rc_recv_info_bit   : std_logic := '0';
  signal rc_recv_msg_strobe : std_logic := '0';

  signal rc_recv_sync_status  : std_logic_vector(2 downto 0);
  signal rc_recv_sync_lost    : std_logic_vector(2 downto 0);
  signal rc_recv_clear_status : std_logic := '0';

  signal rc_recv_tick_ns    : std_logic_vector(63 downto 0) := (others => '0');
  signal rc_recv_sync_err   : std_logic := '1';

  -------------------------
  -- Trigger generation. --
  -------------------------

  signal trig1_period    : unsigned(31 downto 0) := (others => '0');
  signal trig1_counter   : unsigned(31 downto 0) := (others => '0');
  signal trig1_send_cnt  : unsigned(5 downto 0) := (others => '0');
  signal trig1_send_gen  : std_logic := '0';

  signal trig3_period    : unsigned(31 downto 0) := (others => '0');
  signal trig3_counter   : unsigned(31 downto 0) := (others => '0');
  signal trig3_send_cnt  : unsigned(5 downto 0) := (others => '0');
  signal trig3_send_gen  : std_logic := '0';

  signal trig_dead_cnt   : unsigned(7 downto 0) := (others => '0');
  signal trig_dead       : std_logic := '0';

  signal trig_external   : std_logic := '0';

  signal trig1_mux       : std_logic := '0';
  signal trig1_ppl       : std_logic_vector(4 downto 0) := (others => '0');
  signal trig1_le        : std_logic := '0';

  signal trig3_mux       : std_logic := '0';
  signal trig3_ppl       : std_logic_vector(4 downto 0) := (others => '0');
  signal trig3_le        : std_logic := '0';

  signal trig13_or       : std_logic := '0';

  signal trig_prng_lfsr  : std_logic_vector(15 downto 0) := (others => '1');

  ----------------------------------
  -- Streaming data output (LMD). --
  ----------------------------------

  -- Interface signals.
  signal ev_regacc_data_rd    : std_logic_vector(31 downto 0);
  signal ev_regacc_done       : std_logic;
  -- Setup registers.
  signal ev_wr_regs           : word32_array(5 downto 0);
  signal ev_wr_regs_strobe    : std_logic_vector(ev_wr_regs'range);
  -- Read/info/status registers.
  signal ev_rd_regs           : word32_array(4 downto 0);
  -- Counters.
  signal ev_counters          : std_logic_vector(15 downto 0);

  -- Signals from event generation (typically readout).
  signal lmd_event_word       : std_logic_vector(31 downto 0);
  signal lmd_event_offset     : std_logic_vector(4 downto 0);
  signal lmd_event_write      : std_logic;
  signal lmd_event_commit_len : std_logic_vector(4 downto 0);
  signal lmd_event_commit     : std_logic;
  signal lmd_event_free       : std_logic;
  signal lmd_event_reset      : std_logic;

  -- For LMD formatting.
  signal lmd_subev_type_subtype      : std_logic_vector(31 downto 0);
  signal lmd_subev_procid_crate_ctrl : std_logic_vector(31 downto 0);
  signal lmd_subev_ts_id             : std_logic_vector( 7 downto 0);
  signal lmd_gen_ts                  : std_logic;

  -- Force output of the current data buffer.
  signal lmd_flush            : std_logic := '0';
  -- Data buffer signals.
  signal lmd_data_word       : std_logic_vector(31 downto 0);
  signal lmd_data_offset     : std_logic_vector(9 downto 0);
  signal lmd_data_write      : std_logic;
  signal lmd_data_commit_len : std_logic_vector(10 downto 0);
  signal lmd_data_commit     : std_logic;
  signal lmd_data_free       : std_logic;
  signal lmd_data_reset      : std_logic;

  -- Info/status from LMD formatting.
  signal lmd_info_bufno      : std_logic_vector(31 downto 0);
  signal lmd_info_evcnt      : std_logic_vector(31 downto 0);

begin

  -------------------------------
  -- Control resetting the PHY --
  -------------------------------
  process(clk)
  begin
    if (rising_edge(clk)) then
      if (phy_reset_counter(phy_reset_counter'high) = '0') then
        phy_reset_counter <= phy_reset_counter + 1;
      end if;
      eth_rstn <=
        phy_reset_counter(phy_reset_counter'high) or
        phy_reset_counter(phy_reset_counter'high-1);
    end if;
  end process;

  ----------------------
  -- MDIO to/from PHY --
  ----------------------

  eth_mdc  <= mdc_out;
  eth_mdio <= mdio_out when (mdio_ena = '1') else 'Z';

  mdio_in  <= eth_mdio;

  ---------------------------------------
  -- Get (pseudo-unique) DNA from FPGA --
  ---------------------------------------

  -- DNA_PORT is slow, so needs a slow clock.
  dna_get: entity work.efb_xilinx_dna_port
    generic map(nbits => 57)
    port map(
      clk              => clk25,
      dna              => fpga_dna
      );

  ----------
  -- XADC --
  ----------

  xadc_data_request             <= timeout_counter_tick;
  mon_data_array(off_xadc_data) <= xadc_data_array(0);
  mon_has_data(off_xadc_data)   <= xadc_has_data;
  xadc_data_pending             <= mon_data_pending(off_xadc_data);

  --------------------
  -- SPI flash read --
  --------------------

  spi_read: entity work.efb_spi_flash_read
    port map (
      clk          => clk,

      spi_sdi      => spi_sdi,

      spi_csn      => spi_csn,
      spi_sdo      => spi_sdo,
      spi_wpn      => spi_wpn,
      spi_hldn     => spi_hldn,
      spi_sck      => spi_sck,

      i_addr       => spir_addr,
      i_start      => spir_start,

      o_data       => spir_data,
      o_has_data   => spir_has_data
      );

  spi_read_mux: entity work.efb_spi_read_mux
    port map (
      clk          => clk,

      o_addr       => spir_addr,
      o_start      => spir_start,

      i_data       => spir_data,
      i_has_data   => spir_has_data,

      i_a_addr     => spir_cfg_addr,
      i_a_start    => spir_cfg_start,

      o_a_data     => spir_cfg_data,
      o_a_has_data => spir_cfg_has_data,

      i_b_addr     => spir_reg_addr,
      i_b_start    => spir_reg_start,

      o_b_data     => spir_reg_data,
      o_b_has_data => spir_reg_has_data);

  text_config: entity work.efb_spi_text_config
    port map (
      clk            => clk,

      i_base_addr    => spi_cfg_base_addr,

      o_spi_addr     => spir_cfg_addr,
      o_spi_start    => spir_cfg_start,

      i_spi_data     => spir_cfg_data,
      i_spi_has_data => spir_cfg_has_data,

      -- Output value (one at a time).
      o_value        => cfg_value,
      o_value_index  => cfg_value_index,
      o_has_value    => cfg_has_value,
      i_value_done   => cfg_value_done,
      i_value_used   => cfg_value_used,
      -- Overall completion status.
      o_done         => cfg_done,
      o_fail         => cfg_fail,
      o_fail_offset  => cfg_fail_offset,
      o_fail_code    => cfg_fail_code,
      -- Debug output.
      o_dbg_has_data_pend => cfg_dbg_has_data_pend,
      o_dbg_reset_parse   => cfg_dbg_reset_parse,
      o_dbg_active        => cfg_dbg_active,
      o_dbg_state_code    => cfg_dbg_state_code
      );

  process(clk)
  begin
    if (rising_edge(clk)) then
      spir_p1_csn <= spi_csn;
      spir_p1_sdo <= spi_sdo;
      spir_p1_sdi <= spi_sdi;
      spir_p1_sck <= spi_sck;

      spir_p2_csn <= spir_p1_csn;
      spir_p2_sdo <= spir_p1_sdo;
      spir_p2_sdi <= spir_p1_sdi;
      spir_p2_sck <= spir_p1_sck;

      spir_cnt_csn <= spir_cnt_csn + ("" & (spir_p2_csn and not spir_p1_csn));
      spir_cnt_sdo <= spir_cnt_sdo + ("" & (spir_p2_sdo and not spir_p1_sdo));
      spir_cnt_sdi <= spir_cnt_sdi + ("" & (spir_p2_sdi and not spir_p1_sdi));
      spir_cnt_sck <= spir_cnt_sck + ("" & (spir_p2_sck and not spir_p1_sck));

    end if;
  end process;

  -------------------
  -- Cycle counter --
  -------------------

  cycle_count_next <= cycle_count + 1;
  cycle_count_wrap <= cycle_count xor cycle_count_next;

  process (clk)
  begin
    if (rising_edge(clk)) then
      -- PPS monitoring.
      cycle_count <= cycle_count_next;
    end if;
  end process;

  o_cycle_count <= cycle_count;

  --------------
  -- PMOD GPS --
  --------------

  gps_gen: if (num_pmod_gps > 0) generate
    pmod_gps: for i in 0 to num_pmod_gps-1 generate
      pmod_gps_i: entity work.efb_pmod_gps
        generic map(
          clk_freq         => clk_freq,
          pps_nsubsamples  => pmod_gps_pps_nsubsampl
          )
        port map(
          clk              => clk,
          pin_3dfix        => pmod_gps_3dfix(i), -- From GPS.
          pin_tx           => open,      -- ja1, -- To GPS.
          pin_rx           => pmod_gps_rx(i),    -- From GPS.

          pps_subsamples   =>                    -- From GPS+sampler.
            pmod_gps_pps_samples((i+1) * (pmod_gps_pps_nsubsampl+2)-1 downto
                                 i     * (pmod_gps_pps_nsubsampl+2)),

          o_pps_pulse      => gps_pps_pulse(i),

          o_ntp_leap       => ntp_leap(i),
          o_ntp_prec       => ntp_prec(i),
          o_ntp_root_disp  => ntp_root_disp(i),
          o_ntp_cur_ts     => ntp_cur_ts(i),
          o_ntp_ref_ts     => ntp_ref_ts(i),

          i_mon_cycle_count => std_logic_vector(cycle_count),
          i_mon_gps_no     => i+1,
          o_mon_data_array => gps_mon_data_array(22+i*14 to 22+i*14+13),
          o_mon_has_data   => gps_mon_has_data(22+i*14 to 22+i*14+13),
          i_mon_data_pending => gps_mon_data_pending(22+i*14 to 22+i*14+13)
          );
      pmod_gps_tx(i) <= gps_uart_tx;
    end generate;

    ntp_leap_0      <= ntp_leap(0);
    ntp_prec_0      <= ntp_prec(0);
    ntp_root_disp_0 <= ntp_root_disp(0);
    ntp_cur_ts_0    <= ntp_cur_ts(0);
    ntp_ref_ts_0    <= ntp_ref_ts(0);

    -- NTPQ monitor output (reports queries and responses).
    -- We use the GPS # 0, i.e. 0000 in the high nibble.
    --
    -- 00000001 00000001 ........ ........ : NTP query
    -- ipv4ipv4 ipv4ipv4 ipv4ipv4 ipv4ipv4 : NTP query sent to IPV4
    -- tmtmtmtm tmtmtmtm tmtmtmtm tmtmtmtm : NTP query transmit timestamp sent
    -- tmtmtmtm tmtmtmtm tmtmtmtm tmtmtmtm : NTP query tmit ts sent (low)
    -- ssssssss ssssssss ssssssss ssssssss : NTP time seconds (s)
    -- ffffffff ffffffff ffffffff ffffffff : NTP time fractional seconds (f)
    --
    -- 00000001 00000010 ........ ........ : NTP response
    -- ipv4ipv4 ipv4ipv4 ipv4ipv4 ipv4ipv4 : NTP response from IPV4
    -- ssssssss ssssssss ssssssss ssssssss : NTP time seconds (s)
    -- ffffffff ffffffff ffffffff ffffffff : NTP time fractional seconds (f)
    -- Followed by 12 32-bit words with the NTP packet data.

    gps_mon_data_array(0) <= "00000001" & "0000" & "0001" &
                             "00000000" & "00000000";
    gps_mon_data_array(1) <= ntpq_ip;
    gps_mon_data_array(2) <= ntpq_tm_hi;
    gps_mon_data_array(3) <= ntpq_tm_lo;
    gps_mon_data_array(4) <= ntp_cur_ts_0(63 downto 32);
    gps_mon_data_array(5) <= ntp_cur_ts_0(31 downto  0);
    gps_mon_has_data(0 to 5) <= (others => (ntpq_sent and
                                            not gps_mon_data_pending(5)));

    gps_mon_data_array(6) <= "00000001" & "0000" & "0010" &
                             "00000000" & "00000000";
    gps_mon_data_array(7) <= ntpr_ip;
    gps_mon_data_array(8) <= ntpr_recv_ts(63 downto 32);
    gps_mon_data_array(9) <= ntpr_recv_ts(31 downto  0);
    ntpr_data_copy: for i in 0 to 11 generate -- i.e. 10..21
      gps_mon_data_array(10+i) <= ntpr_data(i);
    end generate;
    gps_mon_has_data(6 to 21) <= (others => (ntpr_got and
                                             not gps_mon_data_pending(21)));

    mon_data_array(off_gps_mon_data to
                   off_gps_mon_data+num_gps_mon_data-1) <= gps_mon_data_array;
    mon_has_data  (off_gps_mon_data to
                   off_gps_mon_data+num_gps_mon_data-1) <= gps_mon_has_data;
    gps_mon_data_pending <=
      mon_data_pending(off_gps_mon_data to
                       off_gps_mon_data+num_gps_mon_data-1);

    gps_uart_tx_c : entity work.efnet_uart_tx
      port map(
        clk             => clk,
        i_bit_period    => std_logic_vector(to_unsigned(clk_freq / 9600, 16)),
        i_data          => gps_uart_tx_data,
        i_has_data      => gps_uart_tx_hasdata,
        o_taken         => gps_uart_tx_taken,
        o_tx            => gps_uart_tx_temp
        );

    gps_uart_tx <= not gps_uart_tx_temp;
  end generate;

  mon_data_array(off_sampler_data to
                 off_sampler_data+num_sampler_data-1) <= sampler_data_array;
  mon_has_data  (off_sampler_data to
                 off_sampler_data+num_sampler_data-1) <= sampler_has_data;
  sampler_data_pending <=
    mon_data_pending(off_sampler_data to
                     off_sampler_data+num_sampler_data-1);

  sampler_pulse_control <= sampler_pulse_control_int;

  mon_data_inject: entity work.efnet_data_array_inject
    port map(
      clk             => clk,

      data_array      => mon_data_array,   -- gps_monx_data_array,
      has_data        => mon_has_data,     -- gps_monx_has_data,
      pending         => mon_data_pending, -- gps_monx_data_pending,

      data_word       => mon_data_word,
      data_offset     => mon_data_offset,
      data_write      => mon_data_write,
      data_commit_len => mon_data_commit_len,
      data_commit     => mon_data_commit,
      data_free       => mon_data_free,
      data_reset      => mon_data_reset
      );

  ---------------------------
  -- Rataclock timestamps. --
  ---------------------------

  process(clk)
  begin
    if (rising_edge(clk)) then
      -- For this to work, the period better be whole ns.
      timestamp_ns <= timestamp_ns + (1000000000 / clk_freq);
    end if;
  end process;

  -- Slow Rataclock sender (free running, no eight-slot).
  send_clock_slow : entity work.rataser_clock_send
    generic map(period_bits => 6)
    port map(
      clk                => clk,

      tick_ns            => std_logic_vector(timestamp_ns),
      aux_sigs           => rc_send_aux_sigs,
      info_bit           => rc_send_info_bit,

      pulse_period_clks  => "100000", -- 32, 3.9 MHz output pulse
      duty_low_max_clks  => "010100", -- 20
      duty_low_min_clks  => "001010", -- 10
      eight_slot         => '0',

      pulse_period_ns    => "0100000000", -- 256 ns
      use_pulse_period_ns => '1',

      message_delay_ns   => (others => '0'),

      transmit           => rc_send_serial,
      transmit_sync      => rc_send_serial_sync,

      trail_short        => open,
      trail_long         => open
      );

  rataclock_send <= rc_send_serial;

  rc_recv_serial <=
    rataclock_recv when (trig_external = '1') else rc_send_serial;

  -- Rataclock receiver (no eight-slot).
  recv_clock : entity work.rataser_clock_recv
    generic map(period_bits => 6,
                num_last_edges => 1)
    port map(
      clk               => clk,
      clk90             => '0',

      receive           => rc_recv_serial,
      eight_slot        => '0',

      expect_edge       => "00",
      last_edges        => open,
      use_auto_edge     => '1',
      auto_edge         => open,

      receive_delay_ns  => (others => '0'),

      tick_ns_lo        => rc_recv_tick_ns_lo,
      tick_ns_hi        => rc_recv_tick_ns_hi,
      aux_sigs          => rc_recv_aux_sigs,
      info_bit          => rc_recv_info_bit,
      msg_strobe        => rc_recv_msg_strobe,

      sync_status       => rc_recv_sync_status,
      sync_lost         => rc_recv_sync_lost,
      bad_signals       => open,
      clear_status      => rc_recv_clear_status,

      pulse_period_clks => (others => '0'),
      clk_period_ns     => (others => '0')
      );

  rc_recv_tick_ns(31 downto  0) <= rc_recv_tick_ns_lo;
  rc_recv_tick_ns(63 downto 32) <= rc_recv_tick_ns_hi;

  rc_recv_sync_err <= '0' when (rc_recv_sync_status = "111") else '1';

  -------------------------
  -- Trigger generation. --
  -------------------------

  process(clk)
  begin
    if (rising_edge(clk)) then
      trig1_counter <= trig1_counter - 1;
      trig3_counter <= trig3_counter - 1;

      trig1_send_gen <= '0';
      trig3_send_gen <= '0';
      trig_dead <= '0';

      if (trig1_send_cnt /= 0) then
        trig1_send_gen <= '1';
        trig1_send_cnt <= trig1_send_cnt - 1;
      end if;
      if (trig3_send_cnt /= 0) then
        trig3_send_gen <= '1';
        trig3_send_cnt <= trig3_send_cnt - 1;
      end if;

      if (trig_dead_cnt /= 0) then
        trig_dead <= '1';
        trig_dead_cnt <= trig_dead_cnt - 1;
      end if;

      if (trig1_counter = 0) then
        trig1_counter <= trig1_period;
        if (trig_dead = '0' and lmd_event_free = '1') then
          trig1_send_cnt <=
            to_unsigned(16,6) + (unsigned(trig_prng_lfsr(1 downto 0)) & "000");
          trig_dead_cnt <= to_unsigned(200, trig_dead_cnt'length);
          trig_dead <= '1';
        end if;
      elsif (trig3_counter = 0) then
        trig3_counter <= trig3_period;
        if (trig_dead = '0' and lmd_event_free = '1') then
          trig3_send_cnt <=
            to_unsigned(16,6) + (unsigned(trig_prng_lfsr(1 downto 0)) & "000");
          trig_dead_cnt <= to_unsigned(200, trig_dead_cnt'length);
          trig_dead <= '1';
        end if;
      end if;

      -- Antimetastability and leading edge detector.
      trig1_ppl <= trig1_mux & trig1_ppl(4 downto 1);
      trig3_ppl <= trig3_mux & trig3_ppl(4 downto 1);

      -- Pseudo-random generator.
      trig_prng_lfsr(15 downto 1) <= trig_prng_lfsr(14 downto 0);
      trig_prng_lfsr(0) <= trig_prng_lfsr(15) xor trig_prng_lfsr(13);
    end if;
  end process;

  trig1_send <= trig1_send_gen;
  trig3_send <= trig3_send_gen;

  trig1_mux <= trig1_recv when (trig_external = '1') else trig1_send_gen;
  trig3_mux <= trig3_recv when (trig_external = '1') else trig3_send_gen;

  trig1_le <= '1' when (trig1_ppl(2 downto 0) = "100") else '0';
  trig3_le <= '1' when (trig3_ppl(2 downto 0) = "100") else '0';

  trig13_or <= trig1_ppl(2) or trig3_ppl(2);

  ----------------------------------
  -- Streaming data output (LMD). --
  ----------------------------------

  ev_reg_cnt: entity work.efnet_reg_counters
    generic map(
      base_wr_regs     => 16#1000#,
      base_rd_regs     => 16#1100#,
      base_counters    => 16#1200#)
    port map(
      clk              => clk,
      -- Register access interface
      reg_addr         =>    regacc_addr,
      reg_data_wr      =>    regacc_data_wr,
      reg_data_rd      => ev_regacc_data_rd,
      reg_write        =>    regacc_write,
      reg_read         =>    regacc_read,
      reg_done         => ev_regacc_done,
      reg_cnt          =>    regacc_cnt,
      --
      wr_regs          => ev_wr_regs,
      wr_regs_strobe   => ev_wr_regs_strobe,
      rd_regs          => ev_rd_regs,
      counters         => ev_counters
      );

  sim_event: entity work.efb_sim_events
    port map (
      clk              => clk,
      -- Generation options.
      gen_ts           => lmd_gen_ts,
      -- Timestamp.
      timestamp_ns     => rc_recv_tick_ns,
      timestamp_err    => rc_recv_sync_err,
      -- Triggers.
      trig1            => trig1_le,
      trig3            => trig3_le,
      trig13_or        => trig13_or,
      trig_external    => trig_external,
      -- Events.
      event_word       => lmd_event_word,
      event_offset     => lmd_event_offset,
      event_write      => lmd_event_write,
      event_commit_len => lmd_event_commit_len,
      event_commit     => lmd_event_commit,
      event_free       => lmd_event_free,
      event_reset      => lmd_event_reset
      );

  -- Address map:
  --
  -- 0x1000 =         Subtype, type.
  -- 0x1001 =         Ctrl, crate, procid.
  -- 0x1002 =         TS gen (8), TS ID (7..0).
  -- 0x1003 =         Trigger 1 period.
  -- 0x1004 =         Trigger 3 period.
  -- 0x1005 =         External trigger.

  lmd_subev_type_subtype      <= ev_wr_regs(0);
  lmd_subev_procid_crate_ctrl <= ev_wr_regs(1);
  lmd_subev_ts_id             <= ev_wr_regs(2)(7 downto 0);
  lmd_gen_ts                  <= ev_wr_regs(2)(8);

  trig1_period                <= unsigned(ev_wr_regs(3));
  trig3_period                <= unsigned(ev_wr_regs(4));
  trig_external               <= ev_wr_regs(5)(0);

  lmd_flush <= timeout_counter_tick;

  lmd_format: entity work.efb_lmd_buffer_events
    port map (
      clk              => clk,
      -- Event buffer.
      event_word       => lmd_event_word,
      event_offset     => lmd_event_offset,
      event_write      => lmd_event_write,
      event_commit_len => lmd_event_commit_len,
      event_commit     => lmd_event_commit,
      event_free       => lmd_event_free,
      event_reset      => lmd_event_reset,
      -- Semi-static info.
      subev_type_subtype      => lmd_subev_type_subtype,
      subev_procid_crate_ctrl => lmd_subev_procid_crate_ctrl,
      subev_ts_id             => lmd_subev_ts_id,
      -- Keep-alive.
      flush            => lmd_flush,
      -- To output buffer.
      data_word        => lmd_data_word,
      data_offset      => lmd_data_offset,
      data_write       => lmd_data_write,
      data_commit_len  => lmd_data_commit_len,
      data_commit      => lmd_data_commit,
      data_free        => lmd_data_free,
      data_reset       => lmd_data_reset,
      -- Info
      info_bufno       => lmd_info_bufno,
      info_evcnt       => lmd_info_evcnt
      );

  ev_rd_regs(0) <= lmd_info_bufno;
  ev_rd_regs(1) <= lmd_info_evcnt;
  ev_rd_regs(2) <= rc_recv_tick_ns_lo;
  ev_rd_regs(3) <= rc_recv_tick_ns_hi;
  ev_rd_regs(4) <=
    "00000000" & "00000000" &
    "00000" & rc_recv_sync_lost &
    "00000" & rc_recv_sync_status;

  --------------------------------------
  -- Fakernet control/helper signals. --
  --------------------------------------

  -- Counters for slow and timeout tick signals to Fakernet.
  process (clk)
  begin
    if (rising_edge(clk)) then
      slow_counter <= slow_counter + 1;
      if (slow_counter = "1111111") then
        slow_counter_tick <= '1';
      else
        slow_counter_tick <= '0';
      end if;

      timeout_counter <= timeout_counter + 1;
      if (timeout_counter = (timeout_counter'range => '0')) then
        timeout_counter_tick <= '1';
      else
        timeout_counter_tick <= '0';
      end if;
    end if;
  end process;

  cfg_fixed_ip  <= '1' when (dynamic_gen > 0) else '1';
  cfg_dyn_ip    <= '1' when (dynamic_gen > 0) else '0';
  cfg_gen_rarp  <= '1' when (dynamic_gen > 0) else '0';
  cfg_gen_bootp <= '0' when (dynamic_gen > 0) else '0';
  cfg_gen_dhcp  <= '1' when (dynamic_gen > 0) else '0';
  cfg_gen_ntpq  <= '1' when (dynamic_gen > 0) else '0';

  cfg_udp_mdio     <= '1' when (dynamic_gen > 0) else '0';
  cfg_udp_testctrl <= '1' when (dynamic_gen > 0) else '0';

  -- WARNING: FPGA_DNA is NOT an EUI-48, and it is also not
  -- necessarily unique (may be same on 32 devices), so using it as a
  -- MAC address, which must be unique in a broadcast-domain is at
  -- best a hack!

  -- Set MAC address bits from fpga_dna and user switches for low two bits.
  macaddr( 1 downto  0) <= sw_latch(1 downto 0);
  macaddr( 3 downto  2) <= (others => '0'); -- Could also be from switches.
  macaddr(39 downto  4) <= fpga_dna(35 downto  0);
  macaddr(41 downto 40) <= "10"; -- Locally administered and unicast bits.
  macaddr(47 downto 42) <= fpga_dna(41 downto 36);

  -- Get values from SPI flash text config.
  process (clk)
  begin
    if (rising_edge(clk)) then
      cfg_value_done <= '0';
      cfg_value_used <= '0';
      if (cfg_has_value = '1') then
        -- We consider the address.
        cfg_value_done <= '1';
        if (cfg_value_index = tcfg_index_fixed_ip) then
          tcfg_ipaddr <= cfg_value(tcfg_ipaddr'range);
          tcfg_has_ipaddr <= '1';
          cfg_value_used <= '1';
        end if;
        if (cfg_value_index = tcfg_index_mac) then
          tcfg_macaddr <= cfg_value(tcfg_macaddr'range);
          tcfg_has_macaddr <= '1';
          cfg_value_used <= '1';
        end if;
      end if;
    end if;
  end process;

  -- Use values from SPI flash text config when given.
  sel_macaddr <= tcfg_macaddr when (tcfg_has_macaddr = '1') else macaddr;
  sel_ipaddr  <= tcfg_ipaddr  when (tcfg_has_ipaddr  = '1') else cfg_ipaddr;

  -- Simulated data generation.
  -- We try to commit data into the data buffer every slow tick.
  -- (Note that data is written every cycle.)
  sim_data_word       <= (0 => '1', 31 downto 1 => '0');
  sim_data_offset     <= (9 downto 0 => '0');
  sim_data_write      <= '1';
  sim_data_commit_len <= (4 => '1', 10 downto 5 => '0', 3 downto 0 => '0');
  sim_data_commit     <= slow_counter_tick and sim_data_free;

  -- Choose data source for tcp.
  data_word <=
    mon_data_word when (data_source = "00") else
    lmd_data_word when (data_source = "10") else
    sim_data_word;
  data_offset <=
    mon_data_offset when (data_source = "00") else
    lmd_data_offset when (data_source = "10") else
    sim_data_offset;
  data_write <=
    mon_data_write when (data_source = "00") else
    lmd_data_write when (data_source = "10") else
    sim_data_write;
  data_commit_len <=
    mon_data_commit_len when (data_source = "00") else
    lmd_data_commit_len when (data_source = "10") else
    sim_data_commit_len;
  data_commit <=
    mon_data_commit when (data_source = "00") else
    lmd_data_commit when (data_source = "10") else
    sim_data_commit;

  mon_data_free <= data_free;
  lmd_data_free <= data_free;
  sim_data_free <= data_free;

  mon_data_reset <= data_reset;
  lmd_data_reset <= data_reset;
  sim_data_reset <= data_reset;

  --------------
  -- Fakernet --
  --------------

  fakernet: entity work.fakernet_module
    generic map(data_bufsize_addrbits => 13,
                compiletime => compiletime,
                description => description)
    port map(
      clk             => clk,
      -- config
      cfg_macaddr     => sel_macaddr,
      cfg_ipaddr      => sel_ipaddr,
      cfg_fixed_ip    => cfg_fixed_ip,
      cfg_dyn_ip      => cfg_dyn_ip,
      cfg_gen_rarp    => cfg_gen_rarp,
      cfg_gen_bootp   => cfg_gen_bootp,
      cfg_gen_dhcp    => cfg_gen_dhcp,
      cfg_gen_ntpq    => cfg_gen_ntpq,
      cfg_udp_mdio    => cfg_udp_mdio,
      cfg_udp_testctrl=> cfg_udp_testctrl,
      -- Input network traffic
      in_word         => in_word,
      in_got_word     => in_got_word,
      in_new_packet   => in_new_packet,
      -- Output network traffic
      out_word        => out_word,
      out_taken       => out_taken,
      out_ena         => out_ena,
      out_payload     => out_payload,
      -- MDIO interface
      mdc_out         => mdc_out,
      mdc_ena         => mdc_ena,
      mdio_in         => mdio_in,
      mdio_out        => mdio_out,
      mdio_ena        => mdio_ena,
      -- Register access interface
      reg_addr        => regacc_addr,
      reg_data_wr     => regacc_data_wr,
      reg_data_rd     => regacc_data_rd,
      reg_write       => regacc_write,
      reg_read        => regacc_read,
      reg_done        => regacc_done,
      reg_cnt         => regacc_cnt,
      -- Data input interface
      data_word       => data_word,
      data_offset     => data_offset,
      data_write      => data_write,
      data_commit_len => data_commit_len,
      data_commit     => data_commit,
      data_free       => data_free,
      tcp_reset       => data_reset,
      -- NTP
      ntp_leap        => ntp_leap_0,
      ntp_prec        => ntp_prec_0,
      ntp_root_delay  => (others => '0'),
      ntp_root_disp   => ntp_root_disp_0,
      ntp_cur_ts      => ntp_cur_ts_0,
      ntp_ref_ts      => ntp_ref_ts_0,
      -- NTP query report.
      ntpq_req        => ntpq_req,
      ntpq_mac        => ntpq_mac,
      ntpq_ip         => ntpq_ip,
      ntpq_tm_hi      => ntpq_tm_hi,
      ntpq_tm_lo      => ntpq_tm_lo,
      ntpq_sent       => ntpq_sent,
      -- NTP response report.
      ntpr_got        => ntpr_got,
      ntpr_ip         => ntpr_ip,
      ntpr_recv_ts    => ntpr_recv_ts,
      ntpr_data       => ntpr_data,
      -- Ticker, to be ~10 times per max length packet send period.
      slow_clock_tick => slow_counter_tick,
      timeout_tick    => timeout_counter_tick,
      -- Debug
      debug_in_info_counts  => in_info,
      debug_out_info_counts => out_info,
      debug_state_in  => debug_state_in,
      debug_state_out => debug_state_out,
      debug_state_regacc => open
      );

  ----------------
  -- UART trace --
  ----------------

  trace_signals(15 downto 0) <= in_word;
  trace_signals(16) <= in_got_word;
  trace_signals(17) <= in_new_packet;
  trace_insert  <= in_got_word or in_new_packet;
  trace_trigger <= in_new_packet;

  -- Trace debug of text config parsing.
  --
  -- trace_signals( 7 downto 0)  <= spir_cfg_data;
  -- trace_signals( 8)           <= spir_cfg_has_data;
  -- trace_signals( 9)           <= spir_cfg_start;
  -- trace_signals(13 downto 10) <= spir_cfg_addr(3 downto 0);
  -- trace_signals(14)           <= cfg_dbg_has_data_pend;
  -- trace_signals(15)           <= cfg_dbg_reset_parse;
  -- trace_signals(20 downto 16) <= cfg_dbg_state_code;
  -- trace_signals(21)           <= cfg_dbg_active;
  -- trace_signals(22)           <= '0';
  -- trace_signals(24 downto 23) <= cfg_value_index(1 downto 0);
  -- trace_signals(25)           <= cfg_has_value;
  -- trace_signals(29 downto 26) <= cfg_fail_code;
  -- trace_signals(30)           <= cfg_fail;
  -- trace_signals(31)           <= cfg_done;
  --
  -- trace_insert <= -- '1' or
  --   spir_has_data or spir_start or
  --   cfg_dbg_has_data_pend or cfg_dbg_reset_parse or cfg_has_value or
  --   cfg_fail or cfg_done;
  -- trace_trigger <= trace_insert;

  uart_ctrl : entity work.efb_uart_trace_mem
    generic map(
      width => 32,
      samples => 512)
    port map(
      clk             => clk,
      i_signals       => trace_signals,
      i_insert        => trace_insert,
      i_trigger       => trace_trigger,
      o_data          => trace_tx_data,
      i_taken         => uart_tx_taken
      );
  trace_tx_has_data <= '1';

  ------------------
  -- UART control --
  ------------------

  -- uart_tx_data     <= trace_tx_data;
  -- uart_tx_has_data <= trace_tx_has_data;
  uart_tx_data     <= infoc_tx_data;
  uart_tx_has_data <= infoc_tx_has_data;
  --uart_tx_data     <= slip_tx_data;
  --uart_tx_has_data <= slip_tx_has_data;

  -- The serial-USB chip is on the board, so the UART speed is not
  -- constrained by external serial cabling.
  uart_tx_c : entity work.efnet_uart_tx
    port map(
      clk             => clk,
      --                 125000000/9600   = 13020 , 100000000/9600   = 10416
      --                 125000000/115200 =  1085 , 100000000/115200 =   868
      --                 125000000/230400 =   542 , 100000000/230400 =   434
      --                 125000000/460800 =   271 , 100000000/460800 =   217
      --                 125000000/921600 =   135 , 100000000/921600 =   108
      i_bit_period    => std_logic_vector(to_unsigned(clk_freq / 921600, 16)),
      i_data          => uart_tx_data,
      i_has_data      => uart_tx_has_data,
      o_taken         => uart_tx_taken,
      o_tx            => uart_tx_temp
      );

  uart_tx <= not uart_tx_temp;

  uart_rx_temp <= not uart_rx;

  uart_rx_c : entity work.efnet_uart_rx
    port map(
      clk             => clk,
      --                 See efnet_uart_tx.
      i_bit_period    => std_logic_vector(to_unsigned(clk_freq / 921600, 16)),
      i_rx            => uart_rx_temp,
      o_data          => uart_rx_data,
      o_has_data      => uart_rx_has_data
      );

  -- uart_mon_data_array(0) <=
  --   "00000100" & "000000" &
  --   (uart_tx_has_data and uart_tx_taken) & uart_rx_has_data &
  --   uart_tx_data & uart_rx_data;
  -- uart_mon_has_data(0) <=
  --   (uart_tx_has_data and uart_tx_taken) or uart_rx_has_data;

  mon_data_array(off_uart_data) <= uart_mon_data_array(0);
  mon_has_data(off_uart_data)   <= uart_mon_has_data(0);
  uart_mon_data_pending(0)      <= mon_data_pending(off_uart_data);

  parse_slip: entity work.efnet_slip_rx
    port map (
      clk           => clk,

      i_data        => uart_rx_data,
      i_has_data    => uart_rx_has_data,

      o_word_1      => slip_in_word(15 downto  8),
      o_word_2      => slip_in_word( 7 downto  0),
      o_words_ready => slip_in_got_word,
      o_packet_start => slip_in_new_packet
      );

  send_slip: entity work.efnet_slip_tx
    port map (
      clk           => clk,

      out_word      => slip_out_word,
      out_payload   => slip_out_payload,
      out_crc       => slip_out_crc,
      out_taken     => slip_out_taken,

      o_data        => slip_tx_data,
      o_has_data    => slip_tx_has_data,
      i_taken       => uart_tx_taken
      );

  ----------------
  -- Fakernet 2 --
  ----------------

  fakernet2: entity work.fakernet_module
    generic map (data_bufsize_addrbits => 13,
                 compiletime => compiletime,
                 description => description)
    port map (
      ------------------------ INPUTS -------------------------
      clk           => clk,

      ------------------------ CONFIG -------------------------
      cfg_macaddr   => (others => '0'),
      cfg_ipaddr    => (others => '0'),
      cfg_fixed_ip  => '1',

      ------------------------ INPUTS -------------------------
      in_word       => slip_in_word,
      in_got_word   => slip_in_got_word,
      in_new_packet => slip_in_new_packet,
      in_slip_frame => '1',

      ------------------------ OUTPUTS ------------------------
      out_word      => slip_out_word,
      out_ena       => slip_out_ena,
      out_payload   => slip_out_payload,
      out_crc       => slip_out_crc,
      out_taken     => slip_out_taken,

      ------------------------ MDIO ---------------------------
      mdio_in   => '0',

      ------------------------ REGISTER ACCESS ----------------
      reg_data_rd => (others => '0'),
      reg_done    => '0',

      ------------------------ DATA INPUT ---------------------
      data_write      => '0',
      data_word       => (others => '0'),
      data_offset     => (0 downto 0 => '0'),
      data_commit     => '0',
      data_commit_len => (0 downto 0 => '0'),

      ------------------------ TICKER -------------------------
      slow_clock_tick => slow_counter_tick,
      timeout_tick   => timeout_counter_tick
      );

  ----------------------------
  -- Button and LED control --
  ----------------------------

  -- Address map:
  --
  -- 0x0001 =    1     Switches & buttons
  -- 0x0002 =    2     Fixed LED.
  -- 0x0003 =    3     RGB (LED) time-on fraction.
  -- 0x0004 =    4     Sampler pulse control.
  -- 0x0005 =    5     Data source for TCP buffer.
  -- 0x0006 =    6     SPI flash read debug counters.
  -- 0x0007 =    7     SPI flash read (one byte).
  -- 0x0008 =    8     GPS UART write
  -- 0x0009 =    9     Text config next SPI address return.
  -- 0x000a =   10     Text config last SPI read byte.
  -- 0x000b =   11     Text config last value return.
  -- 0x0010 =   16-21  NTPQ fire.

  infoc_out_udp <= fnet_or_reduction(out_info.udp_idp & out_info.udp);

  rgb_led_stretch <= rgb_led_info1 or rgb_led_info2 or rgb_led_info3;

  process (clk)
  begin
    if (rising_edge(clk)) then
      -- Default read value.
      regacc_data_rd <= (others => '0');
      regacc_done <= '0';

      regacc_data_rd <= ev_regacc_data_rd;
      regacc_done    <= ev_regacc_done;

      -- Latch the button and switch values.
      sw_latch <= sw;
      btn_latch <= btn;

      ------------------------------------------------------------------
      -- Dirty hack to disable UDP control.
      if (dynamic_gen > 0) then

      -- Read which switches are selected and which buttons are pressed.
      if (regacc_read = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00000001") -- 0x0001 / 1
      then
        regacc_data_rd <= (31 downto 8 => '0') & sw_latch & btn_latch;
        regacc_done <= '1';
      end if;

      -- Control which LEDs are on.
      if (regacc_write = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00000010") -- 0x0002 / 2
      then
        rgb_led_stat_r <= regacc_data_wr( 3 downto  0);
        rgb_led_stat_g <= regacc_data_wr( 7 downto  4);
        rgb_led_stat_b <= regacc_data_wr(11 downto  8);
        rgb_led_pkt_info <= regacc_data_wr(12);
        regacc_done <= '1';
      end if;

      -- Fraction of time control.
      if (regacc_write = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00000011") -- 0x0003 / 3
      then
        rgb_frac <= regacc_data_wr(15 downto  0);
        regacc_done <= '1';
      end if;

      -- Sampler pulser control (first channel extra flips).
      if (regacc_write = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00000100") -- 0x0004 / 4
      then
        sampler_pulse_control_int <= regacc_data_wr(31 downto  0);
        regacc_done <= '1';
      end if;

      -- Data source control.
      if (regacc_write = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00000101") -- 0x0005 / 5
      then
        data_source <= regacc_data_wr(data_source'range);
        regacc_done <= '1';
      end if;

      spir_reg_start <= '0';
      -- SPI flash read (issue the access, i.e. write from outside).
      if (regacc_write = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00000111") -- 0x0007 / 7
      then
        spir_reg_addr <= regacc_data_wr(23 downto  0);
        spir_reg_start <= '1';
        spir_reg_has_data_latched <= '0'; -- Data not valid until we get a
                                      -- response.
        regacc_done <= '1';
      end if;

      if (spir_reg_has_data = '1') then
        spir_reg_has_data_latched <= '1'; -- Data was read from flash.
      end if;

      -- SPI flash read (get the value, i.e. read from outside).
      if (regacc_read = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00000111") -- 0x0007 / 7
      then
        regacc_data_rd <=
          '1' & (30 downto 9 => '0') &
          spir_reg_has_data_latched & spir_reg_data;
        regacc_done <= '1';
      end if;

      if (regacc_read = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00000110") -- 0x0006 / 6
      then
        regacc_data_rd <= std_logic_vector(spir_cnt_csn) &
                          std_logic_vector(spir_cnt_sdo) &
                          std_logic_vector(spir_cnt_sdi) &
                          std_logic_vector(spir_cnt_sck);
        regacc_done <= '1';
      end if;

      if (cfg_has_value = '1') then
        cfg_last_has_value   <= '1';
        cfg_last_value       <= cfg_value;
        cfg_last_value_index <= cfg_value_index;
      end if;

      if (regacc_read = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00001001") -- 0x0009 / 9
      then
        regacc_data_rd <=
          cfg_done & cfg_fail & "00" & cfg_fail_code &
          spir_cfg_addr;
        regacc_done <= '1';
      end if;

      if (spir_cfg_has_data = '1') then
        spir_cfg_has_data_last <= '1';
        spir_cfg_data_last     <= spir_cfg_data;
      end if;

      if (regacc_read = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00001010") -- 0x000a / 10
      then
        regacc_data_rd <=
          cfg_done & cfg_fail & "00" & cfg_fail_code &
          "00000000" & "0000000" & spir_cfg_has_data_last &
          spir_cfg_data_last;
        regacc_done <= '1';
      end if;

      if (regacc_read = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00001011") -- 0x000b / 11
      then
        regacc_data_rd <=
          cfg_done & cfg_fail & "00" & cfg_fail_code &
          cfg_last_has_value & "001000" & cfg_last_value_index(8 downto 0) &
          cfg_last_value(7 downto 0);
        regacc_done <= '1';
      end if;

      if (num_pmod_gps > 0) then
        -- GPS UART write.
        if (gps_uart_tx_taken = '1') then
          gps_uart_tx_hasdata <= '0';
        end if;

        if (regacc_write = '1' and
            regacc_addr(15 downto 0) = "00000000" & "00001000") -- 0x0008 / 8
        then
          gps_uart_tx_data <= regacc_data_wr(7 downto 0);
          gps_uart_tx_hasdata <= '1';
          regacc_done <= '1';
        end if;
      end if;

      -- Write register, addr 16 : fire NTPQ request
      --                      17 : NTPQ IP  31.. 0
      --                      18 : NTPQ TM  63..32
      --                      19 : --
      --                      20 : NTPQ MAC 47..32
      --                      21 : NTPQ MAC 31.. 0
      ntpq_req <= '0';
      if (regacc_write = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00010000") then -- 0x0010 / 16
        ntpq_req <= '1';
        regacc_done <= '1';
      end if;
      if (regacc_write = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00010001") then -- 0x0011 / 17
        ntpq_ip <= regacc_data_wr;
        regacc_done <= '1';
      end if;
      if (regacc_write = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00010010") then -- 0x0012 / 18
        ntpq_tm_hi <= regacc_data_wr;
        regacc_done <= '1';
      end if;
      if (regacc_write = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00010011") then -- 0x0013 / 19
        regacc_done <= '1';
      end if;
      if (regacc_write = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00010100") then -- 0x0014 / 20
        ntpq_mac(47 downto 32) <= regacc_data_wr(15 downto 0);
        regacc_done <= '1';
      end if;
      if (regacc_write = '1' and
          regacc_addr(15 downto 0) = "00000000" & "00010101") then -- 0x0015 / 21
        ntpq_mac(31 downto  0) <= regacc_data_wr;
        regacc_done <= '1';
      end if;

      end if;
      -- End of dirty disable hack. ----
      ------------------------------------------------------------------

      -- UART packet info counts.

      -- Send (if any) the current character.
      infoc_tx_data <= "01" & std_logic_vector(infoc_cycle);
      -- If the current output slot is pending, then request transmission.
      infoc_tx_has_data <= infoc_pending(to_integer(infoc_cycle));

      -- Move the search along if the current slot has no data.
      -- Always move one step ahead if data was taken (to make sure
      -- that we do not just emit one character even if there is a
      -- high rate of those counts).
      infoc_cycle <=
        infoc_cycle + ("" & (uart_tx_taken or
                             (not infoc_pending(to_integer(infoc_cycle)))));

      -- If UART took the data, then clear the pending flag,
      -- and continue the search.
      if (uart_tx_taken = '1') then
        for i in infoc_pending'range loop
          if (i = to_integer(infoc_cycle)) then
            infoc_pending(i) <= '0';
          end if;
        end loop;
      end if;
      -- The expensive part of infoc_pending is the cleaning above.
      -- But unused entries (which never can be set to 1 below) ought
      -- to be optimized away by synthesis, so are cheap.

      -- 0         1         2         3          4         5         6
      -- 01234567890123456789012345678901 23456789012345678901234567890123
      -- -ABCDEFGHIJKLMNOPQRSTUVWXYZ----_ -abcdefghijklmnopqrstuvwxyz-----

      -- in:  a arp r rarp i icmp u udp t tcp b bootp d dhcp n ntp
      -- out: A arp+icmp+ntp P pktgen(rarp,bootp,dhcp,ntpq) U udp T tcp
      --   (reserve INRBD for distinguishing sent packets)

      if (in_info.timeout_tick = '1')   then infoc_pending(31) <= '1'; end if;
      if (in_info.good_arp = '1')       then infoc_pending(33) <= '1'; end if;
      if (in_info.good_rarp = '1')      then infoc_pending(50) <= '1'; end if;
      if (in_info.good_icmp = '1')      then infoc_pending(41) <= '1'; end if;
      if (in_info.good_udp = '1')       then infoc_pending(53) <= '1'; end if;
      if (in_info.good_tcp = '1')       then infoc_pending(52) <= '1'; end if;
      if (in_info.good_bootp = '1')     then infoc_pending(34) <= '1'; end if;
      if (in_info.good_ntp = '1')       then infoc_pending(46) <= '1'; end if;

      if (out_info.arp_icmp = '1')      then infoc_pending( 1) <= '1'; end if;
      if (out_info.pkt_gen = '1')       then infoc_pending(16) <= '1'; end if;
      if (infoc_out_udp = '1')          then infoc_pending(21) <= '1'; end if;
      if (out_info.tcp = '1')           then infoc_pending(20) <= '1'; end if;

      -- 2^21 = ~2000000 = 20 ms.  With the 3 stages for stretch,
      -- gives 2x-3x, i.e. 40-60 ms blinks.
      if (cycle_count_wrap(21) = '1') then
        rgb_led_info3 <= rgb_led_info2;
        rgb_led_info2 <= rgb_led_info1;
        rgb_led_info1 <= (others => '0');
      end if;

      -- R
      if (in_info.good_arp   = '1')   then rgb_led_info1( 0) <= '1'; end if;
      if (in_info.good_icmp  = '1')   then rgb_led_info1( 1) <= '1'; end if;
      if (in_info.good_ntp   = '1')   then rgb_led_info1( 2) <= '1'; end if;
      if (out_info.arp_icmp  = '1')   then rgb_led_info1( 3) <= '1'; end if;
      -- G
      if (in_info.good_udp   = '1')   then rgb_led_info1( 4) <= '1'; end if;
      if (infoc_out_udp      = '1')   then rgb_led_info1( 5) <= '1'; end if;
      if (in_info.good_tcp   = '1')   then rgb_led_info1( 6) <= '1'; end if;
      if (out_info.tcp       = '1')   then rgb_led_info1( 7) <= '1'; end if;
      -- B
      if (in_info.good_bootp = '1')   then rgb_led_info1( 8) <= '1'; end if;
      if (in_info.good_rarp  = '1')   then rgb_led_info1( 9) <= '1'; end if;
      if (out_info.pkt_gen   = '1')   then rgb_led_info1(10) <= '1'; end if;
      if (pmod_gps_pps_samples(0) =
                               '1')   then rgb_led_info1(11) <= '1'; end if;

      -- Counter for fraction of time.
      rgb_counter <= rgb_counter + '1';

      -- Default values.
      led_r_ppl <= (others => '0');
      led_g_ppl <= (others => '0');
      led_b_ppl <= (others => '0');

      -- Only light the LEDs a fraction of the time.
      if (rgb_counter < rgb_frac) then
        if (rgb_led_pkt_info = '0') then
          led_r_ppl <= rgb_led_stat_r;
          led_g_ppl <= rgb_led_stat_g;
          led_b_ppl <= rgb_led_stat_b;
        else
          led_r_ppl <= rgb_led_stretch( 3 downto 0);
          led_g_ppl <= rgb_led_stretch( 7 downto 4);
          led_b_ppl <= rgb_led_stretch(11 downto 8);
        end if;
      end if;
    end if;
  end process;

  -- Count the incoming packets, and show count with user LEDs.
  process (clk)
  begin
    if (rising_edge(clk)) then
      if (in_new_packet = '1') then
        packet_counter <= packet_counter + 1;
      end if;

      led_ppl <= pmod_gps_pps_samples(0) & pmod_gps_rx(0) &
                 packet_counter(1 downto 0);

      led   <= led_ppl;
      led_r <= led_r_ppl;
      led_g <= led_g_ppl;
      led_b <= led_b_ppl;
    end if;
  end process;

end RTL;
