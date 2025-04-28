`timescale 1ns / 1ps

module hls4ml_wrapper_tb;

// Clock and Reset
reg clk;
reg rst;

// Wires to connect wrapper and IP
wire [3199:0] hls4ml_input_data_flat;
wire [127:0] hls4ml_output_data_flat;
wire hls4ml_start;
wire hls4ml_done;
wire fifo_empty;
wire fifo_full;

// Clock generation
initial begin
    clk = 0;
    forever #5 clk = ~clk; // 100MHz clock
end

// Reset generation
initial begin
    rst = 1;
    #20;
    rst = 0;
end

// Instantiate hls4ml_wrapper
hls4ml_wrapper uut_wrapper (
    .clk(clk),
    .rst(rst),
    .waveform_data_in(waveform_data_in),
    .waveform_wr_en(waveform_wr_en),
    .user_data_word(data_word),
    .user_data_offset(data_offset),
    .user_data_write(data_write),
    .user_data_commit(data_commit),
    .user_data_free(1'b1),  // Assume always free in TB
    .hls4ml_input_data_flat(hls4ml_input_data_flat),
    .hls4ml_output_data_flat(hls4ml_output_data_flat),
    .hls4ml_start(hls4ml_start),
    .hls4ml_done(hls4ml_done),
    .fifo_empty(fifo_empty),
    .fifo_full(fifo_full)
);

// Instantiate dummy_hls4ml_ip
// (This module just echoes something back after a few cycles)
dummy_hls4ml_ip uut_hls4ml_ip (
    .ap_clk(clk),
    .ap_rst_n(~rst),
    .ap_start(hls4ml_start),
    .ap_done(hls4ml_done),
    .ap_idle(),
    .ap_ready(),
    .input_data_flat(hls4ml_input_data_flat),
    .output_data_flat(hls4ml_output_data_flat)
);

// Driving input waveform
reg [31:0] waveform_data_in;
reg waveform_wr_en;

initial begin
    waveform_data_in = 0;
    waveform_wr_en = 0;
    #30;

    // Send 100 samples
    repeat (100) begin
        @(posedge clk);
        waveform_data_in <= $random;
        waveform_wr_en <= 1'b1;
    end
    @(posedge clk);
    waveform_wr_en <= 1'b0;

    #1000;
    $finish;
end

// Capturing output
wire [31:0] data_word;
wire [9:0] data_offset;
wire data_write;
wire data_commit;

always @(posedge clk) begin
    if (data_write) begin
        $display("Data write: offset=%d, word=%h", data_offset, data_word);
    end
    if (data_commit) begin
        $display("Data commit triggered.");
    end
end

endmodule
