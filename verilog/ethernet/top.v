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
    reg udp_start;
    udp_sender udp_inst (
        .clk(clk),
        .start(udp_start),
        .udp_data(udp_data),
        .udp_valid(udp_valid)
    );

    // IP ヘッダー生成
    reg ip_start;
    ip_header ip_inst (
        .clk(clk),
        .start(ip_start),
        .ip_data(ip_data),
        .ip_valid(ip_valid)
    );

    // Ethernet フレーム生成 (MAC + EtherType)
    reg eth_start;
    ethernet_frame eth_inst (
        .clk(clk),
        .start(eth_start),
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
    reg [2:0] tx_state;  // ステートマシン
    reg [1:0] crc_count; // CRC送信用カウンタ

    always @(posedge tx_clk or posedge rst) begin
        if (rst) begin
            tx_state        <= 3'b000;
            rgmii_tx_data   <= 8'h00;
            rgmii_tx_valid  <= 1'b0;

            eth_start       <= 1'b0;
            ip_start        <= 1'b0;
            udp_start       <= 1'b0;

            crc_init        <= 1'b0;
            crc_calc        <= 1'b0;
            crc_finish      <= 1'b0;
            crc_count       <= 2'b00;
        end else begin
            case (tx_state)
                // 初期化完了待ち
                3'b000: begin
                    if (mdio_ready) begin
                        eth_start <= 1'b1;  // Ethernetフレームの生成開始
                        crc_init  <= 1'b1;  // CRC初期化
                        tx_state  <= 3'b001;
                    end
                end

                // Ethernetフレーム送信中
                3'b001: begin
                    eth_start <= 1'b0;
                    crc_init  <= 1'b0;

                    if (eth_valid) begin
                        rgmii_tx_valid <= 1'b1;
                        rgmii_tx_data  <= eth_data;
                        crc_calc       <= 1'b1;
                    end else begin
                        rgmii_tx_valid <= 1'b0;
                        crc_calc       <= 1'b0;
                        ip_start       <= 1'b1;   // 次はIPヘッダ
                        tx_state       <= 3'b010;
                    end
                end

                // IPヘッダー送信中
                3'b010: begin
                    ip_start <= 1'b0;

                    if (ip_valid) begin
                        rgmii_tx_valid <= 1'b1;
                        rgmii_tx_data  <= ip_data;
                        crc_calc       <= 1'b1;
                    end else begin
                        rgmii_tx_valid <= 1'b0;
                        crc_calc       <= 1'b0;
                        udp_start      <= 1'b1;   // 次はUDP
                        tx_state       <= 3'b011;
                    end
                end

                // UDPペイロード送信中
                3'b011: begin
                    udp_start <= 1'b0;

                    if (udp_valid) begin
                        rgmii_tx_valid <= 1'b1;
                        rgmii_tx_data  <= udp_data;
                        crc_calc       <= 1'b1;
                    end else begin
                        rgmii_tx_valid <= 1'b0;
                        crc_calc       <= 1'b0;

                        crc_finish     <= 1'b1;   // CRC完了要求
                        crc_count      <= 2'b00;
                        tx_state       <= 3'b100;
                    end
                end

                // CRC送信開始
                3'b100: begin
                    crc_finish <= 1'b0;

                    if (crc_ready) begin
                        rgmii_tx_valid <= 1'b1;
                        rgmii_tx_data  <= crc_value[7:0];  // LSBから送信
                        crc_count      <= 2'b01;
                        tx_state       <= 3'b101;
                    end
                end

                // CRC 2バイト目
                3'b101: begin
                    rgmii_tx_data <= crc_value[15:8];
                    crc_count     <= crc_count + 1;
                    tx_state      <= 3'b110;
                end

                // CRC 3バイト目
                3'b110: begin
                    rgmii_tx_data <= crc_value[23:16];
                    crc_count     <= crc_count + 1;
                    tx_state      <= 3'b111;
                end

                // CRC 4バイト目 → 完了
                3'b111: begin
                    rgmii_tx_data  <= crc_value[31:24];
                    rgmii_tx_valid <= 1'b0;  // 送信終了
                    tx_state       <= 3'b000; // 再度送信待機へ
                end

                default: begin
                    tx_state <= 3'b000;
                end
            endcase
        end
    end

    // RGMII 送信モジュール
    rgmii_tx rgmii_inst (
        .tx_clk(tx_clk),        // 送信クロック
        .tx_data(rgmii_tx_data),
        .tx_valid(rgmii_tx_valid),
        .tx_ctl(rgmii_tx_ctl),
        .txd(rgmii_txd)
    );

    // RGMII 送信クロック出力
    assign rgmii_txc = tx_clk;

endmodule
