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

    //------------------------------------------------
    // ヘッダー生成モジュール用 Start 信号
    //------------------------------------------------
    reg eth_start;
    reg ip_start;
    reg udp_start;

    //------------------------------------------------
    // UDP パケット生成
    //------------------------------------------------
    udp_sender udp_inst (
        .clk(tx_clk),      // クロックを統一
        .start(udp_start), // start制御を改良
        .udp_data(udp_data),
        .udp_valid(udp_valid)
    );

    //------------------------------------------------
    // IP ヘッダー生成
    //------------------------------------------------
    ip_header ip_inst (
        .clk(tx_clk),      // クロックを統一
        .start(ip_start),
        .ip_data(ip_data),
        .ip_valid(ip_valid)
    );

    //------------------------------------------------
    // Ethernet フレーム生成 (MAC + EtherType)
    //------------------------------------------------
    ethernet_frame eth_inst (
        .clk(tx_clk),      // クロックを統一
        .start(eth_start),
        .eth_data(eth_data),
        .eth_valid(eth_valid)
    );

    //------------------------------------------------
    // PHY 初期化 (MDIO 設定)
    //------------------------------------------------
    mdio_ctrl mdio_inst (
        .clk(tx_clk), // クロック統一
        .rst(rst),
        .mdc(mdc),
        .mdio(mdio),
        .ready(mdio_ready)
    );

    //------------------------------------------------
    // CRC32 計算用信号
    //------------------------------------------------
    wire [31:0] crc_value;
    wire crc_ready;

    reg crc_init;
    reg crc_calc;
    reg crc_finish;

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

    //------------------------------------------------
    // 送信データ管理（RGMIIクロックに同期）
    //------------------------------------------------
    reg [7:0] rgmii_tx_data;
    reg rgmii_tx_valid;
    reg [3:0] tx_state;

    always @(posedge tx_clk or posedge rst) begin
        if (rst) begin
            tx_state        <= 4'b0000;
            rgmii_tx_data   <= 8'h00;
            rgmii_tx_valid  <= 1'b0;
            eth_start       <= 1'b0;
            ip_start        <= 1'b0;
            udp_start       <= 1'b0;
            crc_init        <= 1'b0;
            crc_calc        <= 1'b0;
            crc_finish      <= 1'b0;
        end else begin
            case (tx_state)
                //------------------------------------------------
                // PHY準備完了 → Ethernetヘッダー送信開始
                //------------------------------------------------
                4'b0000: begin
                    if (mdio_ready) begin
                        eth_start       <= 1'b1; // 開始
                        crc_init        <= 1'b1; // CRC初期化
                        crc_calc        <= 1'b0;
                        crc_finish      <= 1'b0;
                        tx_state        <= 4'b0001;
                    end
                end
                //------------------------------------------------
                // Ethernetヘッダー送信
                //------------------------------------------------
                4'b0001: begin
                    crc_init <= 1'b0;
                    if (eth_valid) begin
                        rgmii_tx_data  <= eth_data;
                        rgmii_tx_valid <= 1'b1;
                        crc_calc       <= 1'b1;
                    end else begin
                        rgmii_tx_valid <= 1'b0;
                        eth_start      <= 1'b0; // 完了検知でOFF
                        ip_start       <= 1'b1; // 次のIPを開始
                        tx_state       <= 4'b0010;
                    end
                end
                //------------------------------------------------
                // IPヘッダー送信
                //------------------------------------------------
                4'b0010: begin
                    if (ip_valid) begin
                        rgmii_tx_data  <= ip_data;
                        rgmii_tx_valid <= 1'b1;
                        crc_calc       <= 1'b1;
                    end else begin
                        rgmii_tx_valid <= 1'b0;
                        ip_start       <= 1'b0; // 完了
                        udp_start      <= 1'b1; // 次のUDP開始
                        tx_state       <= 4'b0011;
                    end
                end
                //------------------------------------------------
                // UDPヘッダー + ペイロード送信
                //------------------------------------------------
                4'b0011: begin
                    if (udp_valid) begin
                        rgmii_tx_data  <= udp_data;
                        rgmii_tx_valid <= 1'b1;
                        crc_calc       <= 1'b1;
                    end else begin
                        rgmii_tx_valid <= 1'b0;
                        udp_start      <= 1'b0; // 完了
                        crc_finish     <= 1'b1; // CRC確定
                        tx_state       <= 4'b0100;
                    end
                end
                //------------------------------------------------
                // CRC送信待機（計算完了待ち）
                //------------------------------------------------
                4'b0100: begin
                    crc_finish <= 1'b0;
                    crc_calc   <= 1'b0;
                    if (crc_ready) begin
                        rgmii_tx_data  <= crc_value[7:0];
                        rgmii_tx_valid <= 1'b1;
                        tx_state       <= 4'b0101;
                    end else begin
                        rgmii_tx_valid <= 1'b0;
                    end
                end
                //------------------------------------------------
                // CRC残り送信
                //------------------------------------------------
                4'b0101: begin
                    rgmii_tx_data <= crc_value[15:8];
                    tx_state      <= 4'b0110;
                end
                4'b0110: begin
                    rgmii_tx_data <= crc_value[23:16];
                    tx_state      <= 4'b0111;
                end
                4'b0111: begin
                    rgmii_tx_data <= crc_value[31:24];
                    rgmii_tx_valid <= 1'b0;
                    tx_state      <= 4'b0000; // 初期状態に戻る
                end
                default: begin
                    tx_state      <= 4'b0000;
                end
            endcase
        end
    end

    //------------------------------------------------
    // RGMII送信モジュール
    //------------------------------------------------
    rgmii_tx rgmii_inst (
        .tx_clk(tx_clk),
        .tx_data(rgmii_tx_data),
        .tx_valid(rgmii_tx_valid),
        .tx_ctl(rgmii_tx_ctl),
        .txd(rgmii_txd)
    );

    // RGMII 送信クロック出力
    assign rgmii_txc = tx_clk;

endmodule
