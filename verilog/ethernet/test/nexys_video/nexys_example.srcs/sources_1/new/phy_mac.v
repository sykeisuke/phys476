
module phy_mac
(
    input  wire       clk,
    input  wire       clk90,
    input  wire       clk_200,
    input  wire       rst,

    /*
     * GMII interface to MAC
     */
    output wire        mac_gmii_rx_clk,
    output wire        mac_gmii_rx_rst,
    output wire [7:0]  mac_gmii_rxd,
    output wire        mac_gmii_rx_dv,
    output wire        mac_gmii_rx_er,
    output wire        mac_gmii_tx_clk,
    output wire        mac_gmii_tx_rst,
    output wire        mac_gmii_tx_clk_en,
    input  wire [7:0]  mac_gmii_txd,
    input  wire        mac_gmii_tx_en,
    input  wire        mac_gmii_tx_er,

    /*
     * RGMII interface to PHY
     */
    input  wire        phy_rgmii_rx_clk,
    input  wire [3:0]  phy_rgmii_rxd,
    input  wire        phy_rgmii_rx_ctl,
    output wire        phy_rgmii_tx_clk,
    output wire [3:0]  phy_rgmii_txd,
    output wire        phy_rgmii_tx_ctl,
    
    output wire        mii_select_out,
    output wire [1:0]  speed_out
    
);


IDELAYCTRL
idelayctrl_inst
(
    .REFCLK(clk_200),
    .RST(rst),
    .RDY()
);

// IODELAY elements for RGMII interface to PHY
wire [3:0] phy_rgmii_rxd_delay;
wire       phy_rgmii_rx_ctl_delay;

IDELAYE2 #(
    .IDELAY_TYPE("FIXED")
)
phy_rxd_idelay_0
(
    .IDATAIN(phy_rgmii_rxd[0]),
    .DATAOUT(phy_rgmii_rxd_delay[0]),
    .DATAIN(1'b0),
    .C(1'b0),
    .CE(1'b0),
    .INC(1'b0),
    .CINVCTRL(1'b0),
    .CNTVALUEIN(5'd0),
    .CNTVALUEOUT(),
    .LD(1'b0),
    .LDPIPEEN(1'b0),
    .REGRST(1'b0)
);

IDELAYE2 #(
    .IDELAY_TYPE("FIXED")
)
phy_rxd_idelay_1
(
    .IDATAIN(phy_rgmii_rxd[1]),
    .DATAOUT(phy_rgmii_rxd_delay[1]),
    .DATAIN(1'b0),
    .C(1'b0),
    .CE(1'b0),
    .INC(1'b0),
    .CINVCTRL(1'b0),
    .CNTVALUEIN(5'd0),
    .CNTVALUEOUT(),
    .LD(1'b0),
    .LDPIPEEN(1'b0),
    .REGRST(1'b0)
);

IDELAYE2 #(
    .IDELAY_TYPE("FIXED")
)
phy_rxd_idelay_2
(
    .IDATAIN(phy_rgmii_rxd[2]),
    .DATAOUT(phy_rgmii_rxd_delay[2]),
    .DATAIN(1'b0),
    .C(1'b0),
    .CE(1'b0),
    .INC(1'b0),
    .CINVCTRL(1'b0),
    .CNTVALUEIN(5'd0),
    .CNTVALUEOUT(),
    .LD(1'b0),
    .LDPIPEEN(1'b0),
    .REGRST(1'b0)
);

IDELAYE2 #(
    .IDELAY_TYPE("FIXED")
)
phy_rxd_idelay_3
(
    .IDATAIN(phy_rgmii_rxd[3]),
    .DATAOUT(phy_rgmii_rxd_delay[3]),
    .DATAIN(1'b0),
    .C(1'b0),
    .CE(1'b0),
    .INC(1'b0),
    .CINVCTRL(1'b0),
    .CNTVALUEIN(5'd0),
    .CNTVALUEOUT(),
    .LD(1'b0),
    .LDPIPEEN(1'b0),
    .REGRST(1'b0)
);

IDELAYE2 #(
    .IDELAY_TYPE("FIXED")
)
phy_rx_ctl_idelay
(
    .IDATAIN(phy_rgmii_rx_ctl),
    .DATAOUT(phy_rgmii_rx_ctl_delay),
    .DATAIN(1'b0),
    .C(1'b0),
    .CE(1'b0),
    .INC(1'b0),
    .CINVCTRL(1'b0),
    .CNTVALUEIN(5'd0),
    .CNTVALUEOUT(),
    .LD(1'b0),
    .LDPIPEEN(1'b0),
    .REGRST(1'b0)
);

    wire [1:0]  speed;
    wire gtx_rst = rst; 

reg [1:0] speed_reg = 2'b10;
reg mii_select_reg = 1'b0;

(* srl_style = "register" *)
reg [1:0] tx_mii_select_sync = 2'd0;

always @(posedge mac_gmii_tx_clk) begin
    tx_mii_select_sync <= {tx_mii_select_sync[0], mii_select_reg};
end

(* srl_style = "register" *)
reg [1:0] rx_mii_select_sync = 2'd0;

always @(posedge mac_gmii_rx_clk) begin
    rx_mii_select_sync <= {rx_mii_select_sync[0], mii_select_reg};
end

// PHY speed detection
reg [2:0] rx_prescale = 3'd0;

always @(posedge mac_gmii_rx_clk) begin
    rx_prescale <= rx_prescale + 3'd1;
end

(* srl_style = "register" *)
reg [2:0] rx_prescale_sync = 3'd0;

always @(posedge clk) begin
    rx_prescale_sync <= {rx_prescale_sync[1:0], rx_prescale[2]};
end

reg [6:0] rx_speed_count_1 = 0;
reg [1:0] rx_speed_count_2 = 0;

always @(posedge clk) begin
    if (gtx_rst) begin
        rx_speed_count_1 <= 0;
        rx_speed_count_2 <= 0;
        speed_reg <= 2'b10;
        mii_select_reg <= 1'b0;
    end else begin
        rx_speed_count_1 <= rx_speed_count_1 + 1;
        
        if (rx_prescale_sync[1] ^ rx_prescale_sync[2]) begin
            rx_speed_count_2 <= rx_speed_count_2 + 1;
        end

        if (&rx_speed_count_1) begin
            // reference count overflow - 10M
            rx_speed_count_1 <= 0;
            rx_speed_count_2 <= 0;
            speed_reg <= 2'b00;
            mii_select_reg <= 1'b1;
        end

        if (&rx_speed_count_2) begin
            // prescaled count overflow - 100M or 1000M
            rx_speed_count_1 <= 0;
            rx_speed_count_2 <= 0;
            if (rx_speed_count_1[6:5]) begin
                // large reference count - 100M
                speed_reg <= 2'b01;
                mii_select_reg <= 1'b1;
            end else begin
                // small reference count - 1000M
                speed_reg <= 2'b10;
                mii_select_reg <= 1'b0;
            end
        end
    end
end

assign speed = speed_reg;
assign mii_select_out = mii_select_reg;
assign speed_out = speed;

rgmii_phy_if #(
    .TARGET("XILINX"),
    .IODDR_STYLE("IODDR"),
    .CLOCK_INPUT_STYLE("BUFR"),
    .USE_CLK90("TRUE")
)
rgmii_phy_if_inst (
    .clk(clk),
    .clk90(clk90),
    .rst(rst),

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

    .phy_rgmii_rx_clk(phy_rgmii_rx_clk),
    .phy_rgmii_rxd(phy_rgmii_rxd_delay),
    .phy_rgmii_rx_ctl(phy_rgmii_rx_ctl_delay),
    .phy_rgmii_tx_clk(phy_rgmii_tx_clk),
    .phy_rgmii_txd(phy_rgmii_txd),
    .phy_rgmii_tx_ctl(phy_rgmii_tx_ctl),

    .speed(speed)
);



endmodule