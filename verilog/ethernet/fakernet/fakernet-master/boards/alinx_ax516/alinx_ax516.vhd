-- Copyright (c) 2022, Haakan T. Johansson
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

entity alinx_ax516_fakernet is
  generic (compiletime : integer := 1;
           use_pmod_gps : boolean := true);
  port (
    -- On-board clock.
    clk50MHz      : in  std_logic;

    ---- Ethernet PHY:
    ---- Control channel.
    eth_mdc        : out std_logic := '0';
    eth_mdio       : inout std_logic := '0';
    eth_rstn       : out std_logic := '1';
    ---- TX channel.
    eth_gtx_clk    : out std_logic;
    eth_txd        : out std_logic_vector(7 downto 0) := (others => '0');
    eth_tx_en      : out std_logic := '0';
    eth_tx_clk     : in  std_logic;
    ---- RX channel.
    eth_rxd        : in  std_logic_vector(7 downto 0);
    eth_rx_clk     : in  std_logic;
    eth_rx_dv      : in  std_logic;
    eth_rx_err     : in  std_logic;
    ---- Link status.
    eth_col        : in  std_logic;
    eth_crs        : in  std_logic;

    -- SPI interface.
    spi_sdi        : in  std_logic;
    spi_csn        : inout std_logic := '1';
    spi_sdo        : inout std_logic := '1';
    --spi_wpn        : out std_logic := '1';
    --spi_hldn       : out std_logic := '1';
    spi_sck        : inout std_logic := '1';

    ---- User input.
    --btn            : in  std_logic_vector(3 downto 0);

    -- LEDs.
    led            : out std_logic_vector(3 downto 0);

    ---- PMOD
    ja0            : in  std_logic;
    ja1            : out std_logic;
    ja2            : in  std_logic;
    ja3            : in  std_logic;

    -- UART
    uart_rx        : in  std_logic;
    uart_tx        : out std_logic
    );
end alinx_ax516_fakernet;

architecture RTL of alinx_ax516_fakernet is

  -- Board clock frequency.
  constant clk_freq       : integer := 125000000;

  -- Clock signals.
  signal clk              : std_logic;
  signal clk25            : std_logic;
  signal clk125           : std_logic;

  -- c0a801bf = 192.168.1.191
  signal ipaddr  : std_logic_vector(31 downto 0) :=
    "11000000" & "10101000" & "00000001" & "10111111";

  -- LED polarity.
  signal led_tmp        : std_logic_vector(3 downto 0) := (others => '0');

  -- Buffered PHY RX clock.
  signal buf_eth_rx_clk  : std_logic;

  -- PHY mode.
  signal mode_gmii       : std_logic;

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

  -- ETH GTX clock
  signal eth_gtx_clk_mux    : std_logic;
  signal eth_gtx_clk_buf    : std_logic;

  signal cheat_clk : std_logic := '0';

  -- Debug signals.
  signal spy_tx_en          : std_logic;
  signal spy_txd            : std_logic_vector(7 downto 0);

  -- SPI signals handled internally.
  signal spi_wpn  : std_logic := '1';
  signal spi_hldn : std_logic := '1';

  -- PMod GPS
  constant num_pmod_gps     : integer := fnet_if_true(use_pmod_gps, 1, 0);
  constant pmod_gps_pps_nsubsampl : integer := 1;

  -- Note: RX/TX swapped w.r.t. module names, here seen from the FPGA.
  signal pmod_gps_3dfix : std_logic_vector(0 to num_pmod_gps-1); -- From GPS.
  signal pmod_gps_tx    : std_logic_vector(0 to num_pmod_gps-1); -- To GPS.
  signal pmod_gps_rx    : std_logic_vector(0 to num_pmod_gps-1); -- From GPS.
  signal pmod_gps_pps   : std_logic_vector(0 to num_pmod_gps-1); -- From GPS.
  signal pmod_gps_pps_samples : -- After sampler, 0 is most recent sample.
    std_logic_vector(num_pmod_gps * (pmod_gps_pps_nsubsampl+2) - 1 downto 0);
  signal pmod_gps_pps_samples_hist : std_logic_vector(4 downto 0);

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

  -- (No) sampler.
  signal sampler_data_array   : word32_array(0 to -1) :=
    (others => (others=>'0'));
  signal sampler_has_data     : std_logic_vector(0 to -1) :=
    (others => '0');
  signal sampler_data_pending : std_logic_vector(0 to -1) :=
    (others => '0');

begin

  -----------------------
  -- Clock generation. --
  -----------------------

  c: entity work.efb_xilinx_common_clk
    generic map(clkfbout_mult => 20,   -- 50 MHz * 20 -> VCO @ 1000 MHz
                clkin_period  => 10.0, -- ns
                gen_eth_ref_clk => false
                )
    port map(
      i_clk_in       => clk50MHz, -- eth_rx_clk,

      o_clk_in_buf   => cheat_clk,

      o_clk          => clk,
      o_clk25        => clk25,
      o_clk125       => clk125,

      eth_ref_clk    => open
      );

  ------------
  -- Eth RX --
  ------------

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

      o_mode_gmii      => mode_gmii,

      o_word_1         => in_word(15 downto 8),
      o_word_2         => in_word(7 downto 0),
      o_words_ready    => in_got_word,
      o_packet_start   => in_new_packet,
      o_packet_ended   => in_end_packet
      );

  ---------------------------------------------------------------------

  -- Note!  The spy_tx* signals are in the eth_gtx_clk clock domain!

  trace_signals(15 downto 0) <= out_word;
  trace_signals(16) <= out_ena;
  trace_signals(17) <= out_taken;
  trace_signals(23) <= spy_tx_en;         -- WRONG clock domain!
  trace_signals(31 downto 24) <= spy_txd; -- WRONG clock domain!
  trace_insert  <= '1';
  trace_trigger <= out_ena;

  --trace_signals(15 downto 0) <= in_word;
  --trace_signals(16) <= in_got_word;
  --trace_signals(17) <= in_new_packet;
  --trace_signals(23) <= buf_eth_rx_dv;
  --trace_signals(31 downto 24) <= buf_eth_rxd;  
  --trace_insert  <= in_got_word or in_new_packet or buf_eth_rx_dv;
  --trace_trigger <=                in_new_packet or buf_eth_rx_dv;

  trace_mem : entity work.efb_uart_trace_mem
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

  uart_tx_data     <= trace_tx_data;
  uart_tx_has_data <= trace_tx_has_data;

  -- The serial-USB chip is on the board, so the UART speed is not
  -- constrained by external serial cabling.
  uart_tx_c: entity work.efnet_uart_tx
    port map(
      clk             => clk,
      --                 125000000/9600   = 13020 , 100000000/9600   = 10416
      --                 125000000/115200 =  1085 , 100000000/115200 =   868
      --                 125000000/230400 =   542 , 100000000/230400 =   434
      --                 125000000/460800 =   271 , 100000000/460800 =   217
      --                 125000000/921600 =   135 , 100000000/921600 =   108
      i_bit_period    => std_logic_vector(to_unsigned(135, 16)),
      i_data          => uart_tx_data,
      i_has_data      => uart_tx_has_data,
      o_taken         => uart_tx_taken,
      o_tx            => uart_tx_temp
      );

  uart_tx <= not uart_tx_temp;

  ------------
  -- Eth TX --
  ------------

  -- Since RX_CLK may be either 125 or 25 MHz (or 2.5 MHz), we cannot
  -- run it through a PLL, since we do not know the appropriate
  -- multiplier to have a VCO frequency within limits.  (Dynamic
  -- reprogramming is not that funny.  And 2.5 MHz is outside input
  -- limits.)

  -- Instead we source either the RX clock (125 MHz) or the wanted TX
  -- clock (25 MHz / 2.5 MHz).  For the RX clock, this does not
  -- matter, as the emitted data is synchronised to the eth_gtx_clk
  -- signal that we emit.

  eth_gtx_clk_muxg: bufgmux
    port map (i0 =>     eth_tx_clk, -- s = '0'
              i1 =>     eth_rx_clk, -- s = '1'  (or clk125)
              o  => eth_gtx_clk_buf,
              s  => mode_gmii);

  ---- Buffer the TX clock.
  --eth_gtx_clk_bufg: bufg
  --  port map (i => eth_gtx_clk_mux,
  --            o => eth_gtx_clk_buf);

  tx : entity work.efnet_gmii_mii_tx
    port map(
      clk             => clk,
      buf_eth_gtx_clk => eth_gtx_clk_buf,

      eth_gtx_clk     => eth_gtx_clk,
      eth_tx_en       => eth_tx_en,
      eth_txd         => eth_txd,

      spy_tx_en       => spy_tx_en,
      spy_txd         => spy_txd,

      i_mode_gmii     => mode_gmii,

      out_ena         => out_ena,
      out_word        => out_word,
      out_taken       => out_taken
      );

  ----------------------------
  -- Common board top-level --
  ----------------------------

  pmod_gps_3dfix(0) <= ja0;
  -- pmod_gps_tx(0)    <= ja1;
  pmod_gps_rx(0)    <= ja2;
  pmod_gps_pps(0)   <= ja3;

  -- When not using an actual subsampler, we still need to provide
  -- history in index 2 and 1.  And before that do anti-metastable.
  process(clk)
  begin
    if (rising_edge(clk)) then
      pmod_gps_pps_samples_hist <=
        pmod_gps_pps_samples_hist(3 downto 0) & pmod_gps_pps(0);
    end if;
  end process;

  pmod_gps_pps_samples <= pmod_gps_pps_samples_hist(4 downto 2);

  ja1 <= pmod_gps_tx(0);

  t: entity work.efb_common_top
    generic map(compiletime  => compiletime,
                description  => "ALINX AX516",
                num_pmod_gps => num_pmod_gps,
                clk_freq     => clk_freq)
    port map(
      clk            => clk,
      clk25          => clk25,

      -- Using same value as for Arty board.
      spi_cfg_base_addr => std_logic_vector(to_unsigned(16#3c0000#,24)),

      cfg_ipaddr     => ipaddr,

      eth_mdc        => eth_mdc,
      eth_mdio       => eth_mdio,
      eth_rstn       => open, -- eth_rstn,
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
      -- eth_rxerr      => '0',
      eth_col        => eth_col,
      eth_crs        => eth_crs,

      spi_sdi        => spi_sdi,
      spi_csn        => spi_csn,
      spi_sdo        => spi_sdo,
      spi_wpn        => spi_wpn,
      spi_hldn       => spi_hldn,
      spi_sck        => spi_sck,

      sw             => "0000",
      btn            => "0000",

      led            => led_tmp,
      led_r          => open,
      led_g          => open,
      led_b          => open,

      pmod_gps_3dfix => pmod_gps_3dfix,
      pmod_gps_tx    => pmod_gps_tx,
      pmod_gps_rx    => pmod_gps_rx,
      pmod_gps_pps_samples => pmod_gps_pps_samples,

      uart_rx        => uart_rx,
      --uart_tx        => uart_tx,

      sampler_data_array   => sampler_data_array,
      sampler_has_data     => sampler_has_data,
      sampler_data_pending => sampler_data_pending
      );

  -- led <= not led_tmp;
  led(3 downto 1) <= not led_tmp(3 downto 1);
  led(0) <= not mode_gmii;
  
end RTL;
