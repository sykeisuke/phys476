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

library unisim;
use unisim.vcomponents.all;

use work.fnet_records.all; -- For word32_array.
use work.fnet_util_pkg.all; -- For fnet_if_true.

entity arty_a7_35_fakernet is
  generic (compiletime : integer := 1;
           dynamic_gen : integer := 1;
           use_pmod_gps : boolean := true);
  port (
    -- On-board clock.
    clk100MHz      : in  std_logic;

    -- Ethernet PHY:
    -- Control channel.
    eth_mdc        : out std_logic := '0';
    eth_mdio       : inout std_logic := '0';
    eth_rstn       : out std_logic := '1';
    -- TX channel.
    eth_txd        : out std_logic_vector(3 downto 0) := (others => '0');
    eth_tx_en      : out std_logic := '0';
    eth_tx_clk     : in  std_logic;
    -- RX channel.
    eth_rxd        : in  std_logic_vector(3 downto 0);
    eth_rx_clk     : in  std_logic;
    eth_rx_dv      : in  std_logic;
    eth_rxerr      : in  std_logic;
    -- Link status.
    eth_col        : in  std_logic;
    eth_crs        : in  std_logic;
    -- Ref clk.
    eth_ref_clk    : out std_logic;

    -- SPI interface.
    spi_sdi        : in  std_logic;
    spi_csn        : inout std_logic := '1';
    spi_sdo        : inout std_logic := '1';
    --spi_wpn        : out std_logic := '1';
    --spi_hldn       : out std_logic := '1';
    --spi_sck        : inout std_logic := '1';

    -- User input.
    sw             : in  std_logic_vector(3 downto 0);
    btn            : in  std_logic_vector(3 downto 0);

    -- LEDs.
    led            : out std_logic_vector(3 downto 0);
    led_r          : out std_logic_vector(3 downto 0);
    led_g          : out std_logic_vector(3 downto 0);
    led_b          : out std_logic_vector(3 downto 0);

    -- PMOD
    ja0            : in  std_logic;
    ja1            : out std_logic;
    ja2            : in  std_logic;
    ja3            : in  std_logic;

    jd0            : in  std_logic;
    jd1            : out std_logic;

    jd2            : in  std_logic;
    jd3            : out std_logic;

    -- ChipKit
    ck_io0         : out std_logic;
    ck_io1         : out std_logic;
    ck_io2         : out std_logic;

    ck_io4         : out std_logic;
    ck_io5         : out std_logic;
    ck_io6         : out std_logic;

    ck_io8         : in  std_logic;
    ck_io9         : in  std_logic;
    ck_io10        : in  std_logic;

    -- Power measurements
    vsnsvu_p       : in  std_logic;
    vsnsvu_n       : in  std_logic;
    vsns5v0_p      : in  std_logic;
    vsns5v0_n      : in  std_logic;
    isns5v0_p      : in  std_logic;
    isns5v0_n      : in  std_logic;
    isns0v95_p     : in  std_logic;
    isns0v95_n     : in  std_logic;

    -- UART
    uart_rx        : in  std_logic;
    uart_tx        : out std_logic

    );

end arty_a7_35_fakernet;

architecture RTL of arty_a7_35_fakernet is

  -- Board clock frequency.
  constant clk_freq         : integer := 125000000;

  -- Clock signals.
  signal clk                : std_logic;
  signal clk25              : std_logic;

  signal clk500             : std_logic;
  signal clk500_90          : std_logic;
  signal clk250             : std_logic;

  -- c0a801c0 = 192.168.1.192
  signal ipaddr  : std_logic_vector(31 downto 0) :=
    "11000000" & "10101000" & "00000001" & "11000000";

  -- User switch.
  signal sw_latch       : std_logic_vector(3 downto 0);

  -- Buffered PHY RX clock.
  signal buf_eth_rx_clk  : std_logic;

  -- Input network traffic
  signal in_word            : std_logic_vector(15 downto 0) := (others => '0');
  signal in_got_word        : std_logic := '0';
  signal in_new_packet      : std_logic := '0';
  signal in_end_packet      : std_logic := '0';

  -- Output network traffic
  signal out_word           : std_logic_vector(15 downto 0) :=
    std_logic_vector(to_unsigned(16#dead#,16));
  signal out_ena            : std_logic := '0';
  signal out_payload        : std_logic := '0';
  signal out_taken          : std_logic := '0';

  -- Buffered PHY TX clock.
  signal eth_tx_clk_buf     : std_logic;

  -- XADC

  signal xadc_data_request : std_logic := '0';
  signal xadc_data_array   : word32_array(0 to 0) :=
    (others => (others => '0'));
  signal xadc_has_data     : std_logic := '0';
  signal xadc_data_pending : std_logic := '0';

  -- SPI signals handled internally.
  signal spi_sck  : std_logic := '1';
  signal spi_wpn  : std_logic := '1';
  signal spi_hldn : std_logic := '1';

  -- Cycle counter
  signal cycle_count        : unsigned(31 downto 0) := (others => '0');

  -- PMod GPS
  constant num_pmod_gps     : integer := fnet_if_true(use_pmod_gps, 1, 0);
  constant pmod_gps_pps_nsubsampl : integer := 16;

  -- Note: RX/TX swapped w.r.t. module names, here seen from the FPGA.
  signal pmod_gps_3dfix : std_logic_vector(0 to num_pmod_gps-1); -- From GPS.
  signal pmod_gps_tx    : std_logic_vector(0 to num_pmod_gps-1); -- To GPS.
  signal pmod_gps_rx    : std_logic_vector(0 to num_pmod_gps-1); -- From GPS.
  signal pmod_gps_pps   : std_logic_vector(0 to num_pmod_gps-1); -- From GPS.
  signal pmod_gps_pps_samples : -- After sampler, 0 is most recent sample.
    std_logic_vector(num_pmod_gps * (pmod_gps_pps_nsubsampl+2) - 1 downto 0);

  -- Rataclock.
  signal rataclock_send : std_logic;
  signal rataclock_recv : std_logic;

  signal trig1_send     : std_logic;
  signal trig3_send     : std_logic;

  signal trig1_recv     : std_logic;
  signal trig3_recv     : std_logic;

  -- Sampler
  signal sample_input       : std_logic_vector(0 to 1) := (others => '0');

  signal sampler_data_array   : word32_array(0 to 3) :=
    (others => (others=>'0'));
  signal sampler_has_data     : std_logic_vector(0 to 3) :=
    (others => '0');
  signal sampler_data_pending : std_logic_vector(0 to 3) :=
    (others => '0');
  signal sampler_pulse_control : std_logic_vector(31 downto 0) :=
    (others => '0');


  -- Pulser
  signal pulser_output      : std_logic_vector(0 to 1) := (others => '0');

begin

  -----------------------
  -- Clock generation. --
  -----------------------

  c: entity work.efb_xilinx_common_clk
    generic map(clkfbout_mult => 10,   -- 100 MHz * 10 -> VCO @ 1000 MHz
                clkin_period  => 10.0, -- ns
                gen_eth_ref_clk => true
                )
    port map(
      i_clk_in       => clk100MHz,

      o_clk          => clk,
      o_clk25        => clk25,

      o_clk250       => clk250,
      o_clk500       => clk500,
      o_clk500_90    => clk500_90,

      eth_ref_clk    => eth_ref_clk
      );

  process (clk)
  begin
    if (rising_edge(clk)) then
      -- Latch the button and switch values.
      sw_latch <= sw;
    end if;
  end process;

  -- Assign the two low bits of the IP address from user input switches.
  ipaddr(1 downto 0) <= sw_latch(1 downto 0);

  ----------
  -- XADC --
  ----------

  adc_get: entity work.efb_xilinx_xadc
    generic map(vaux_enable => "0000011000000110")
    --                          5432109876543210
    port map(
      clk                => clk,
      i_mon_request      => xadc_data_request,
      o_mon_data_array   => xadc_data_array(0),
      o_mon_has_data     => xadc_has_data,
      i_mon_data_pending => xadc_data_pending,
      vauxp     => (1 => vsns5v0_p,  9 => isns5v0_p,
                    2 => vsnsvu_p,  10 => isns0v95_p,
                    others => '0'),
      vauxn     => (1 => vsns5v0_n,  9 => isns5v0_n,
                    2 => vsnsvu_n,  10 => isns0v95_n,
                    others => '0')
      );

  ---------------------
  -- STARTUPE2 block --
  ---------------------

  -- The Arty board also has a direct pin that could be used to drive
  -- the spi_sck signal.  Note, there can be a 7.5 ns routing delay
  -- for the spi_sck to reach the pin.

  startupe2_inst : startupe2
    generic map (prog_usr => "false")
    port map (
      cfgclk    => open,
      cfgmclk   => open,
      eos       => open,
      preq      => open,
      clk       => '0',
      gsr       => '0',
      gts       => '0',
      keyclearb => '0',
      pack      => '0',
      usrcclko  => spi_sck, -- 1-bit input: user cclk input
      usrcclkts => '0',
      usrdoneo  => '1',
      usrdonets => '1'
      );

  -------------------
  -- Data from PHY --
  -------------------

  -- Buffer the input clock.
  clk_bufg: bufg
    port map (i => eth_rx_clk,
              o => buf_eth_rx_clk);
  -- buf_eth_rx_clk <= cheat_clk;

  rx: entity work.efnet_gmii_mii_rx
    generic map(clk_freq => clk_freq)
    port map(
      clk              => clk,
      buf_eth_rx_clk   => buf_eth_rx_clk,

      eth_rx_dv        => eth_rx_dv,
      eth_rxd          => eth_rxd,

      i_mode_gmii      => '0', -- MII.
      i_mode_gmii_set  => '1', -- Fixed at MII.

      o_word_1         => in_word(15 downto 8),
      o_word_2         => in_word(7 downto 0),
      o_words_ready    => in_got_word,
      o_packet_start   => in_new_packet,
      o_packet_ended   => in_end_packet
      );

  -----------------
  -- Data to PHY --
  -----------------

  -- Board only does MII, so provide TX clk.
  eth_gtx_clk_bufg: bufg
    port map (i => eth_tx_clk,
              o => eth_tx_clk_buf);

  -- Emit 16-bit words as nibbles, based on the PHY tx_clk.
  tx : entity work.efnet_gmii_mii_tx
    port map(
      clk             => clk,
      buf_eth_gtx_clk => eth_tx_clk_buf,

      eth_gtx_clk     => open,
      eth_tx_en       => eth_tx_en,
      eth_txd         => eth_txd,

      i_mode_gmii     => '0', -- MII.

      out_ena         => out_ena,
      out_word        => out_word,
      out_taken       => out_taken
      );

  ----------------------------
  -- Common board top-level --
  ----------------------------

  pmod_gps_3dfix(0) <= ja0;
  --pmod_gps_3dfix(1) <= jb0;
  --pmod_gps_3dfix(2) <= jc0;
  -- pmod_gps_tx(0)    <= ja1;
  -- pmod_gps_tx(1)    <= jb1;
  -- pmod_gps_tx(2)    <= jc1;
  pmod_gps_rx(0)    <= ja2;
  --pmod_gps_rx(1)    <= jb2;
  --pmod_gps_rx(2)    <= jc2;
  pmod_gps_pps(0)   <= ja3;
  --pmod_gps_pps(1)   <= jb3;
  --pmod_gps_pps(2)   <= jc3;

  ja1 <= pmod_gps_tx(0);
  --jb1 <= pmod_gps_tx(1);
  --jc1 <= pmod_gps_tx(2);

  pmod_gps: for i in 0 to num_pmod_gps-1 generate
    pps: entity work.efb_sample_x16
      port map(clk       => clk,
               clk_x2    => clk250,
               clk_x4    => clk500,
               clk_x4_90 => clk500_90,
               i_input   => pmod_gps_pps(i),
               o_samples =>
                 pmod_gps_pps_samples((i+1)*(pmod_gps_pps_nsubsampl+2)-1 downto
                                      i    *(pmod_gps_pps_nsubsampl+2))
               );
  end generate;

  t: entity work.efb_common_top
    generic map(compiletime  => compiletime,
                description  => "Digilent Arty A7-35T",
                dynamic_gen  => dynamic_gen,
                num_pmod_gps => num_pmod_gps,
                pmod_gps_pps_nsubsampl => pmod_gps_pps_nsubsampl,
                clk_freq     => clk_freq)
    port map(
      clk            => clk,
      clk25          => clk25,

      -- Maximum .bit size for Arty 100: 30606304 bits = 0x3a607c.
      spi_cfg_base_addr => std_logic_vector(to_unsigned(16#3c0000#,24)),

      cfg_ipaddr     => ipaddr,

      eth_mdc        => eth_mdc,
      eth_mdio       => eth_mdio,
      eth_rstn       => eth_rstn,
      -- eth_txd        => eth_txd,
      -- eth_tx_en      => eth_tx_en,
      -- eth_tx_clk     => eth_tx_clk,
      out_word       => out_word,
      out_ena        => out_ena,
      out_taken      => out_taken,
      -- eth_rxd        => eth_rxd,
      -- eth_rx_clk     => eth_rx_clk,
      -- eth_rx_dv      => eth_rx_dv,
      in_word        => in_word,
      in_got_word    => in_got_word,
      in_new_packet  => in_new_packet,
      -- eth_rxerr      => eth_rxerr,
      eth_col        => eth_col,
      eth_crs        => eth_crs,

      xadc_data_request => xadc_data_request,
      xadc_data_array   => xadc_data_array,
      xadc_has_data     => xadc_has_data,
      xadc_data_pending => xadc_data_pending,

      spi_sdi        => spi_sdi,
      spi_csn        => spi_csn,
      spi_sdo        => spi_sdo,
      spi_wpn        => spi_wpn,
      spi_hldn       => spi_hldn,
      spi_sck        => spi_sck,

      sw             => sw,
      btn            => btn,

      led            => led,
      led_r          => led_r,
      led_g          => led_g,
      led_b          => led_b,

      pmod_gps_3dfix => pmod_gps_3dfix,
      pmod_gps_tx    => pmod_gps_tx,
      pmod_gps_rx    => pmod_gps_rx,
      pmod_gps_pps_samples => pmod_gps_pps_samples,

      uart_rx        => uart_rx,
      uart_tx        => uart_tx,

      o_cycle_count => cycle_count,

      rataclock_send => rataclock_send,
      rataclock_recv => rataclock_recv,

      trig1_send     => trig1_send,
      trig3_send     => trig3_send,
      trig1_recv     => trig1_recv,
      trig3_recv     => trig3_recv,

      sampler_data_array   => sampler_data_array,
      sampler_has_data     => sampler_has_data,
      sampler_data_pending => sampler_data_pending,

      sampler_pulse_control => sampler_pulse_control
      );

  ck_io0 <= rataclock_send;
  ck_io1 <= trig1_send;
  ck_io2 <= trig3_send;
  ck_io4 <= rataclock_send;
  ck_io5 <= trig1_send;
  ck_io6 <= trig3_send;

  rataclock_recv <= ck_io8;
  trig1_recv     <= ck_io9;
  trig3_recv     <= ck_io10;

  -------------------------------
  -- High-speed input sampling --
  -------------------------------

  sample_input <= jd0 & jd2;

  hs: entity work.efb_sampler
    port map(clk       => clk,
             clk_x2    => clk250,
             clk_x4    => clk500,
             clk_x4_90 => clk500_90,
             i_input   => sample_input,
             i_cycle_count => cycle_count,
             o_mon_data_array   => sampler_data_array,
             o_mon_has_data     => sampler_has_data,
             i_mon_data_pending => sampler_data_pending
             );

  hp: entity work.efb_pulser
    port map(clk       => clk,
             clk_x2    => clk250,
             clk_x4    => clk500,
             clk_x4_90 => clk500_90,
             i_cycle_count => cycle_count,
             i_control => sampler_pulse_control,
             o_output  => pulser_output);

  jd1 <= pulser_output(0);
  jd3 <= pulser_output(1);

end RTL;
