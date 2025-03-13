module top (
    input wire clk,         // 100MHz システムクロック
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

    // Clocking Wizard のインスタンス (100MHz → 125MHz)
    clk_wiz_0 clk_gen (
        .clk_out1(tx_clk),  // 生成される 125MHz クロック
        .reset(rst),
        .locked(locked),
        .clk_in1(clk)       // FPGA の 100MHz システムクロック
    );

    // UDP パケット生成
    udp_sender udp_inst (
        .clk(clk),
        .start(ip_valid),
        .udp_data(udp_data),
        .udp_valid(udp_valid)
    );

    // IP ヘッダー生成
    ip_header ip_inst (
        .clk(clk),
        .start(eth_valid),
        .ip_data(ip_data),
        .ip_valid(ip_valid)
    );

    // Ethernet フレーム生成 (MAC + EtherType)
    ethernet_frame eth_inst (
        .clk(clk),
        .start(mdio_ready),  // PHY 初期化完了後に送信開始
        .eth_data(eth_data),
        .eth_valid(eth_valid)
    );

    // PHY 初期化 (MDIO 設定)
    mdio_ctrl mdio_inst (
        .clk(clk),
        .rst(rst),
        .mdc(mdc),
        .mdio(mdio),
        .ready(mdio_ready)
    );

    // CRC32 計算用信号
    wire [31:0] crc_value;
    wire crc_ready;

    reg crc_init;
    reg crc_calc;
    reg crc_finish;

    // CRC32 モジュール（125MHz クロックドメイン）
    crc32_ethernet crc_inst (
        .clk(tx_clk),
        .rst(rst),
        .data_valid(rgmii_tx_valid),
        .data_in(rgmii_tx_data),
        .crc_init(crc_init),
        .crc_calc(crc_calc),
        .crc_finish(crc_finish),
        .crc_out(crc_value),
        .crc_valid(crc_ready)
    );

    // 送信データの管理（RGMIIクロックに同期）
    reg [7:0] rgmii_tx_data;
    reg rgmii_tx_valid;
    reg [2:0] tx_state;  // CRC送信用に拡張

    always @(posedge tx_clk or posedge rst) begin
        if (rst) begin
            tx_state        <= 3'b000;
            rgmii_tx_data   <= 8'h00;
            rgmii_tx_valid  <= 1'b0;
            crc_init        <= 1'b0;
            crc_calc        <= 1'b0;
            crc_finish      <= 1'b0;
        end else begin
            case (tx_state)
                3'b000: begin
                    if (eth_valid) begin
                        rgmii_tx_data  <= eth_data;
                        rgmii_tx_valid <= 1;
                        crc_init       <= 1;   // CRC初期化
                        crc_calc       <= 1;   // CRC計算開始
                        crc_finish     <= 0;
                        tx_state       <= 3'b001;
                    end else begin
                        rgmii_tx_valid <= 0;
                        crc_calc       <= 0;
                        crc_init       <= 0;
                        crc_finish     <= 0;
                    end
                end
                3'b001: begin
                    crc_init <= 0;  // 初期化終了
                    if (ip_valid) begin
                        rgmii_tx_data  <= ip_data;
                        rgmii_tx_valid <= 1;
                        crc_calc       <= 1;
                        tx_state       <= 3'b010;
                    end
                end
                3'b010: begin
                    if (udp_valid) begin
                        rgmii_tx_data  <= udp_data;
                        rgmii_tx_valid <= 1;
                        crc_calc       <= 1;
                        crc_finish     <= 1;   // CRC計算終了
                        tx_state       <= 3'b011;
                    end
                end
                3'b011: begin
                    crc_finish <= 0;
                    crc_calc   <= 0;
                    if (crc_ready) begin
                        rgmii_tx_data  <= crc_value[7:0];   // CRC下位バイト
                        rgmii_tx_valid <= 1;
                        tx_state       <= 3'b100;
                    end
                end
                3'b100: begin
                    rgmii_tx_data <= crc_value[15:8];
                    tx_state      <= 3'b101;
                end
                3'b101: begin
                    rgmii_tx_data <= crc_value[23:16];
                    tx_state      <= 3'b110;
                end
                3'b110: begin
                    rgmii_tx_data  <= crc_value[31:24];
                    rgmii_tx_valid <= 0;      // 送信終了
                    tx_state       <= 3'b000; // 初期状態へ
                end
            endcase
        end
    end

    // RGMII 送信モジュール
    rgmii_tx rgmii_inst (
        .tx_clk(tx_clk),        // 送信クロック
        .tx_data(rgmii_tx_data),// データ
        .tx_valid(rgmii_tx_valid), // 有効信号
        .tx_ctl(rgmii_tx_ctl),
        .txd(rgmii_txd)
    );

    // RGMII 送信クロック出力
    assign rgmii_txc = tx_clk;

endmodule
