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
    wire locked;

    // CRC32モジュールの信号
    wire [31:0] crc_value;
    wire crc_ready;

    reg crc_init;
    reg crc_calc;
    reg crc_finish;

    // Clocking Wizard のインスタンス
    clk_wiz_0 clk_gen (
        .clk_out1(tx_clk),  // 生成される 125MHz クロック
        .reset(rst),
        .locked(locked),
        .clk_in1(clk)       // FPGA の 100MHz システムクロック
    );

    // UDP パケット生成
    udp_sender udp_inst (
        .clk(tx_clk),
        .start(ip_valid),
        .udp_data(udp_data),
        .udp_valid(udp_valid)
    );

    // IP ヘッダー生成
    ip_header ip_inst (
        .clk(tx_clk),
        .start(eth_valid),
        .ip_data(ip_data),
        .ip_valid(ip_valid)
    );

    // Ethernet フレーム生成 (MAC + EtherType)
    ethernet_frame eth_inst (
        .clk(tx_clk),
        .start(mdio_ready),  // PHY 初期化完了後に送信開始
        .eth_data(eth_data),
        .eth_valid(eth_valid)
    );

    crc32_ethernet crc_inst (
        .clk(tx_clk),
        .rst(rst),
        .data_valid(rgmii_tx_valid),  // 送信データが有効
        .data_in(rgmii_tx_data),      // 実際に送信するデータ
        .crc_init(crc_init),          // 初期化フラグ（パケット開始時にON）
        .crc_calc(crc_calc),          // 計算を続ける時ON
        .crc_finish(crc_finish),      // 計算終了・結果取得時にON
        .crc_out(crc_value),
        .crc_valid(crc_ready)
    );

    // PHY 初期化 (MDIO 設定)
    mdio_ctrl mdio_inst (
        .clk(tx_clk),
        .rst(rst),
        .mdc(mdc),
        .mdio(mdio),
        .ready(mdio_ready)
    );

    // 送信データの管理（RGMIIクロックに同期）
    reg [7:0] rgmii_tx_data;
    reg rgmii_tx_valid;
    reg [1:0] tx_state;

    always @(posedge tx_clk) begin
        rgmii_tx_valid <= 0;
        case (tx_state)
            2'b00: begin
                if (eth_valid) begin
                    rgmii_tx_data  <= eth_data;
                    rgmii_tx_valid <= 1;
                    tx_state <= 2'b01;
                end
            end
            2'b01: begin
                if (ip_valid) begin
                    rgmii_tx_data  <= ip_data;
                    rgmii_tx_valid <= 1;
                    tx_state <= 2'b10;
                end
            end
            2'b10: begin
                if (udp_valid) begin
                    rgmii_tx_data  <= udp_data;
                    rgmii_tx_valid <= 1;
                    tx_state <= 2'b00; // 送信完了後にリセット
                end
            end
        endcase
    end

    // RGMII 送信クロック生成
    assign rgmii_txc = tx_clk;

    // RGMII 送信モジュール
    rgmii_tx rgmii_inst (
        .tx_clk(tx_clk),  // RGMII 送信クロック
        .tx_data(rgmii_tx_data),
        .tx_valid(rgmii_tx_valid),
        .tx_ctl(rgmii_tx_ctl),
        .txd(rgmii_txd)
    );


endmodule
