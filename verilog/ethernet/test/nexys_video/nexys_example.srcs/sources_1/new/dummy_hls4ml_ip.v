module dummy_hls4ml_ip (
    input wire ap_clk,
    input wire ap_rst_n,

    input wire ap_start,
    output reg ap_done,
    output reg ap_idle,
    output reg ap_ready,

    input wire [3199:0] input_data_flat, // 100 x 32bit = 3200bit
    output reg [127:0] output_data_flat   // 4 x 32bit = 128bit
);

reg processing;
reg [3:0] processing_counter;

// State machine
always @(posedge ap_clk or negedge ap_rst_n) begin
    if (!ap_rst_n) begin
        ap_done <= 0;
        ap_idle <= 1;
        ap_ready <= 1;
        processing <= 0;
        processing_counter <= 0;
        output_data_flat <= 0;
    end else begin
        if (ap_start && !processing) begin
            ap_idle <= 0;
            ap_ready <= 0;
            processing <= 1;
            processing_counter <= 0;
        end else if (processing) begin
            if (processing_counter == 30) begin
              // Dummy operation: just sum some inputs
              output_data_flat[ 31:  0] <= input_data_flat[ 31:  0] + input_data_flat[ 63: 32];
              output_data_flat[ 63: 32] <= input_data_flat[ 95: 64] + input_data_flat[127: 96];
              output_data_flat[ 95: 64] <= input_data_flat[159:128] + input_data_flat[191:160];
              output_data_flat[127: 96] <= input_data_flat[223:192] + input_data_flat[255:224];

              ap_done <= 1;
              ap_idle <= 1;
              ap_ready <= 1;
              processing <= 0;
            end else begin
              processing_counter <= processing_counter + 1;
              ap_done <= 0;
            end
        end else begin
            ap_done <= 0;
        end
    end
end

endmodule
