
module top
(
    input  wire       clk,

    /*
     * Ethernet: 1000BASE-T RGMII
     */
    input  wire       phy_rx_clk,
    input  wire [3:0] phy_rxd,
    input  wire       phy_rx_ctl,
    output wire       phy_tx_clk,
    output wire [3:0] phy_txd,
    output wire       phy_tx_ctl,
    output wire       phy_reset_n,
    input  wire       phy_int_n,
    //input  wire       phy_pme_n,
    output wire       phy_mdc,
    inout  wire       phy_mdio
    
);



wire clk_ibufg;

// Internal 125 MHz clock
wire clk_mmcm_out;
wire clk_int;   

wire mmcm_rst = 1'b0;
wire mmcm_locked;
wire mmcm_clkfb;

IBUFG
clk_ibufg_inst(
    .I(clk),
    .O(clk_ibufg)
);

wire clk90_mmcm_out;
wire clk90_int;

wire clk_200_mmcm_out;
wire clk_200_int;


wire clk_25_mmcm_out;
wire clk_25_int;
wire clk_500_mmcm_out;
wire clk_500_int;
wire clk_500_90_mmcm_out;
wire clk_500_90_int;
wire clk_250_mmcm_out;
wire clk_250_int;


// MMCM instance
// 100 MHz in, 125 MHz out
// PFD range: 10 MHz to 550 MHz
// VCO range: 600 MHz to 1200 MHz
// M = 10, D = 1 sets Fvco = 1000 MHz (in range)
// Divide by 8 to get output frequency of 125 MHz
// Need two 125 MHz outputs with 90 degree offset
// Also need 200 MHz out for IODELAY
// 1000 / 5 = 200 MHz
MMCME2_BASE #(
    .BANDWIDTH("OPTIMIZED"),
    .CLKOUT0_DIVIDE_F(8),
    .CLKOUT0_DUTY_CYCLE(0.5),
    .CLKOUT0_PHASE(0),
    .CLKOUT1_DIVIDE(8),
    .CLKOUT1_DUTY_CYCLE(0.5),
    .CLKOUT1_PHASE(90),
    .CLKOUT2_DIVIDE(5),
    .CLKOUT2_DUTY_CYCLE(0.5),
    .CLKOUT2_PHASE(0),
    .CLKOUT3_DIVIDE(40),
    .CLKOUT3_DUTY_CYCLE(0.5),
    .CLKOUT3_PHASE(0),
    .CLKOUT4_DIVIDE(2),
    .CLKOUT4_DUTY_CYCLE(0.5),
    .CLKOUT4_PHASE(0),
    .CLKOUT5_DIVIDE(2),
    .CLKOUT5_DUTY_CYCLE(0.5),
    .CLKOUT5_PHASE(90),
    .CLKOUT6_DIVIDE(4),
    .CLKOUT6_DUTY_CYCLE(0.5),
    .CLKOUT6_PHASE(0),
    .CLKFBOUT_MULT_F(10),
    .CLKFBOUT_PHASE(0),
    .DIVCLK_DIVIDE(1),
    .REF_JITTER1(0.010),
    .CLKIN1_PERIOD(10.0),
    .STARTUP_WAIT("FALSE"),
    .CLKOUT4_CASCADE("FALSE")
)
clk_mmcm_inst (
    .CLKIN1(clk_ibufg),
    .CLKFBIN(mmcm_clkfb),
    .RST(mmcm_rst),
    .PWRDWN(1'b0),
    .CLKOUT0(clk_mmcm_out),
    .CLKOUT0B(),
    .CLKOUT1(clk90_mmcm_out),
    .CLKOUT1B(),
    .CLKOUT2(clk_200_mmcm_out),
    .CLKOUT2B(),
    .CLKOUT3(clk_25_mmcm_out),
    .CLKOUT3B(),
    .CLKOUT4(clk_500_mmcm_out),
    .CLKOUT5(clk_500_90_mmcm_out),
    .CLKOUT6(clk_250_mmcm_out),
    .CLKFBOUT(mmcm_clkfb),
    .CLKFBOUTB(),
    .LOCKED(mmcm_locked)
);


BUFG
clk_bufg_inst (
    .I(clk_mmcm_out),
    .O(clk_int)
);

BUFG
clk90_bufg_inst (
    .I(clk90_mmcm_out),
    .O(clk90_int)
);

BUFG
clk_200_bufg_inst (
    .I(clk_200_mmcm_out),
    .O(clk_200_int)
);

BUFG
clk_250_bufg_inst (
    .I(clk_250_mmcm_out),
    .O(clk_250_int)
);

BUFG
clk_25_bufg_inst (
    .I(clk_25_mmcm_out),
    .O(clk_25_int)
);

BUFG
clk_500_bufg_inst (
    .I(clk_500_mmcm_out),
    .O(clk_500_int)
);

BUFG
clk_500_90_bufg_inst (
    .I(clk_500_90_mmcm_out),
    .O(clk_500_90_int)
);


    wire        mac_gmii_rx_clk;
    wire        mac_gmii_rx_rst;
    wire [7:0]  mac_gmii_rxd;
    wire        mac_gmii_rx_dv;
    wire        mac_gmii_rx_er;
    wire        mac_gmii_tx_clk;
    wire        mac_gmii_tx_rst;
    wire        mac_gmii_tx_clk_en;
    wire [7:0]  mac_gmii_txd;
    wire        mac_gmii_tx_en;
    wire        mac_gmii_tx_er;


phy_mac phy_mac_inst (
    .clk (clk_int),
    .clk90 (clk90_int),
    .clk_200 (clk_200_int),
    .rst (1'b0),

    .mac_gmii_rx_clk(mac_gmii_rx_clk),
    .mac_gmii_rx_rst(mac_gmii_rx_rst),
    .mac_gmii_rxd(mac_gmii_rxd),
    .mac_gmii_rx_dv(mac_gmii_rx_dv),
    .mac_gmii_rx_er(mac_gmii_rx_er),
    .mac_gmii_tx_clk(mac_gmii_tx_clk),
    .mac_gmii_tx_rst(mac_gmii_tx_rst),
    .mac_gmii_tx_clk_en(mac_gmii_tx_clk_en),
    .mac_gmii_txd(mac_gmii_txd),
    .mac_gmii_tx_en(mac_gmii_tx_en),
    .mac_gmii_tx_er(mac_gmii_tx_er),

    .phy_rgmii_rx_clk (phy_rx_clk),
    .phy_rgmii_rxd (phy_rxd),
    .phy_rgmii_rx_ctl (phy_rx_ctl),
    .phy_rgmii_tx_clk (phy_tx_clk),
    .phy_rgmii_txd (phy_txd),
    .phy_rgmii_tx_ctl (phy_tx_ctl),
    
    .mii_select_out (),
    .speed_out ()
    
);

wire [3:0] led;
wire [3:0] led_r;
wire [3:0] led_g;
wire [3:0] led_b;

wire phy_mdc_i;


    wire [7:0] countdebug;
    wire [19:0] statedebug;
    wire [31:0] data_word;
    wire [9:0] data_offset;
    wire [10:0] data_commit_len;
    wire data_write;
    wire data_commit;
    wire data_free;
    wire data_reset;

    data_gen_user data_gen_user_inst (
    
    .clk  (clk_int),
    .event_word       (data_word),
    .event_offset     (data_offset),
    .event_write      (data_write),
    .event_commit_len (data_commit_len),
    .event_commit     (data_commit),
    .event_free       (data_free),
    .event_reset      (data_reset)
    
    );


assign phy_mdc = phy_mdc_i;

wire phy_mdio_out;
assign phy_mdio = phy_mdio_out;

fakernet_top fakernet_top_inst (
    .clk_in (clk_int),
    .clk125_in (clk_int),
    //.clk25_in (clk_25_int),

    .eth_intb (phy_int_n),
    .eth_mdc  (phy_mdc_i),
    .eth_mdio_in (phy_mdio),
    .eth_mdio_out (phy_mdio_out),
    .mdio_i_debug (mdio_i_debug),
    .mdio_o_debug (mdio_o_debug),
    .eth_rstn (phy_reset_n),
    .eth_txd  (mac_gmii_txd),
    .eth_tx_en (mac_gmii_tx_en),
    .eth_tx_clk (mac_gmii_tx_clk),
    .eth_rxd  (mac_gmii_rxd),
    .eth_rx_clk (mac_gmii_rx_clk),
    .eth_rx_dv (mac_gmii_rx_dv),
    .eth_rxerr (1'b0),
    .eth_col (1'b1),
    .eth_crs (1'b1),
    .eth_ref_clk (),

    .spi_sdi  (),
    .spi_csn  (),
    .spi_sdo  (),
    .sw (),
    .btn (),

    .led    (led),
    .led_r  (led_r),
    .led_g  (led_g),
    .led_b  (led_b),
    .ja0    (),
    .ja1    (),
    .ja2    (),
    .ja3    (),
    .jd0    (),
    .jd1    (),
    .jd2    (),
    .jd3    (),
    .uart_rx (),
    .uart_tx (),
    .user_data_word       (data_word),
    .user_data_offset     (data_offset),
    .user_data_write      (data_write),
    .user_data_commit_len (data_commit_len),
    .user_data_commit     (data_commit),
    .user_data_free       (data_free),
    .user_data_reset      (data_reset)

    );



endmodule
