module top (
    input wire clk,         // 125MHz システムクロック
    input wire rst,         // リセットボタン

    // RGMII 送信インターフェース
    output wire [3:0] rgmii_txd,
    output wire rgmii_tx_ctl,
    output wire rgmii_txc,

    // MDIO インターフェース (PHY 設定用)
    output wire mdc,
    inout wire mdio
);

    // 内部信号
    wire mdio_ready;
    wire [7:0] eth_data, ip_data, udp_data;
    wire eth_valid, ip_valid, udp_valid;
    wire tx_clk;

    // PHY 初期化 (MDIO 設定)
    mdio_ctrl mdio_inst (
        .clk(clk),
        .rst(rst),
        .mdc(mdc),
        .mdio(mdio),
        .ready(mdio_ready)
    );
  // Ethernet フレーム生成 (MAC + EtherType)
    ethernet_frame eth_inst (
        .clk(clk),
        .start(mdio_ready),  // PHY 初期化完了後に送信開始
        .eth_data(eth_data),
        .eth_valid(eth_valid)
    );

    // IP ヘッダー生成
    ip_header ip_inst (
        .clk(clk),
        .start(eth_valid),
        .ip_data(ip_data),
        .ip_valid(ip_valid)
    );

    // UDP パケット生成
    udp_sender udp_inst (
        .clk(clk),
        .start(ip_valid),
        .udp_data(udp_data),
        .udp_valid(udp_valid)
    );

    // RGMII 送信モジュール
    rgmii_tx rgmii_inst (
        .clk(clk),
        .tx_clk(tx_clk),  // RGMII 送信クロック (125MHz)
        .tx_data(udp_valid ? udp_data : (ip_valid ? ip_data : eth_data)),
        .tx_valid(udp_valid | ip_valid | eth_valid),
        .txd(rgmii_txd),
        .tx_ctl(rgmii_tx_ctl)
    );

    // RGMII 送信クロック生成
    assign rgmii_txc = clk;

endmodule

