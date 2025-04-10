// ================================
// 1. RGMII DDR Transmitter (using ODDR)
// ================================

module rgmii_tx (
    input wire        tx_clk,       // 125 MHz clock
    input wire [7:0]  tx_data,      // 8-bit transmit data
    input wire        tx_valid,     // Data valid signal
    output wire       tx_ctl,       // RGMII TX_CTL (via ODDR)
    output wire [3:0] txd           // RGMII TXD[3:0] (via ODDR)
);

    wire [3:0] txd_low;
    wire [3:0] txd_high;

    assign txd_low  = tx_valid ? tx_data[3:0] : 4'b0000;
    assign txd_high = tx_valid ? tx_data[7:4] : 4'b0000;

    // TX_CTL (transmit valid signal on both edges)
    ODDR #(
        .DDR_CLK_EDGE("SAME_EDGE"),
        .INIT(1'b0),
        .SRTYPE("SYNC")
    ) ODDR_tx_ctl (
        .Q(tx_ctl),
        .C(tx_clk),
        .CE(1'b1),
        .D1(tx_valid),  // Rising edge
        .D2(tx_valid),  // Falling edge
        .R(1'b0),
        .S(1'b0)
    );

    // TXD[3:0] transmitted using ODDR for DDR signaling
    genvar i;
    generate
        for (i = 0; i < 4; i = i + 1) begin : gen_txd
            ODDR #(
                .DDR_CLK_EDGE("SAME_EDGE"),
                .INIT(1'b0),
                .SRTYPE("SYNC")
            ) ODDR_txd (
                .Q(txd[i]),
                .C(tx_clk),
                .CE(1'b1),
                .D1(txd_low[i]),  // Rising edge data
                .D2(txd_high[i]), // Falling edge data
                .R(1'b0),
                .S(1'b0)
            );
        end
    endgenerate

endmodule
