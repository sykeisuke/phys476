// ================================
// 1. RGMII DDR送信 (ODDR使用)
// ================================

module rgmii_tx (
    input wire        tx_clk,       // 125MHz クロック
    input wire [7:0]  tx_data,      // 送信データ 8bit
    input wire        tx_valid,     // 有効信号
    output wire       tx_ctl,       // RGMII TX_CTL (ODDR)
    output wire [3:0] txd           // RGMII TXD[3:0] (ODDR)
);

    wire [3:0] txd_low;
    wire [3:0] txd_high;

    assign txd_low  = tx_valid ? tx_data[3:0] : 4'b0000;
    assign txd_high = tx_valid ? tx_data[7:4] : 4'b0000;

    // TX_CTL (valid信号を両エッジで送る)
    ODDR #(
        .DDR_CLK_EDGE("SAME_EDGE"),
        .INIT(1'b0),
        .SRTYPE("SYNC")
    ) ODDR_tx_ctl (
        .Q(tx_ctl),
        .C(tx_clk),
        .CE(1'b1),
        .D1(tx_valid),  // 立ち上がりでtx_valid
        .D2(tx_valid),  // 立ち下がりもtx_valid
        .R(1'b0),
        .S(1'b0)
    );

    // TXD 4ビット分 ODDRでDDR化
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
                .D1(txd_low[i]),  // 立ち上がりエッジ
                .D2(txd_high[i]), // 立ち下がりエッジ
                .R(1'b0),
                .S(1'b0)
            );
        end
    endgenerate

endmodule

