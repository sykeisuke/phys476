module fifo_test (
    input clk,
    input reset,
    output error_led,            // エラー発生時に点灯
    output [7:0] debug_data_out, // 読み出しデータのデバッグ用出力（任意）
    // FIFO状態のモニタ出力
    output fifo_full,
    output fifo_empty
);

    // FIFO内部の信号
    wire [7:0] fifo_data_out;
    wire fifo_full_internal;
    wire fifo_empty_internal;
    
    // 書き込み側信号
    reg wr_en;
    reg [7:0] write_counter;
    
    // 読み出し側信号
    reg rd_en;
    reg [7:0] expected_read_counter;
    
    // エラーフラグ
    reg error_flag;
    
    // FIFOモジュールのインスタンス
    fifo_sync FIFO_INST (
        .clk(clk),
        .reset(reset),
        .wr_en(wr_en),
        .data_in(write_counter),
        .full(fifo_full_internal),
        .rd_en(rd_en),
        .data_out(fifo_data_out),
        .empty(fifo_empty_internal)
    );
    
    // 状態出力の割り当て
    assign fifo_full = fifo_full_internal;
    assign fifo_empty = fifo_empty_internal;
    assign error_led = error_flag;
    assign debug_data_out = fifo_data_out;
    
    // 書き込み側ロジック
    // FIFOが空きある場合、連番データを生成して書き込む
    always @(posedge clk or posedge reset) begin
        if (reset) begin
            write_counter <= 8'd0;
            wr_en <= 1'b0;
        end else begin
            if (!fifo_full_internal) begin
                wr_en <= 1'b1;
                write_counter <= write_counter + 1;
            end else begin
                wr_en <= 1'b0;
            end
        end
    end
    
    // 読み出し側ロジック
    // FIFOから読み出したデータと、期待する連番を比較
    always @(posedge clk or posedge reset) begin
        if (reset) begin
            rd_en <= 1'b0;
            expected_read_counter <= 8'd0;
            error_flag <= 1'b0;
        end else begin
            if (!fifo_empty_internal) begin
                rd_en <= 1'b1;
                // FIFOから読み出されたデータと期待値を比較
                if (fifo_data_out != expected_read_counter)
                    error_flag <= 1'b1;  // 不一致ならエラー
                expected_read_counter <= expected_read_counter + 1;
            end else begin
                rd_en <= 1'b0;
            end
        end
    end

endmodule;
