module dummy_hls4ml_ip (
    input wire ap_clk,
    input wire ap_rst_n,

    input wire ap_start,
    output reg ap_done,
    output reg ap_idle,
    output reg ap_ready,

    input wire [31:0] input_data[0:99], // 100 input
    output reg [31:0] output_data[0:3]  // 4 output
);

reg processing;

// State machine
always @(posedge ap_clk or negedge ap_rst_n) begin
    if (!ap_rst_n) begin
        ap_done <= 0;
        ap_idle <= 1;
        ap_ready <= 1;
        processing <= 0;
        output_data[0] <= 0;
        output_data[1] <= 0;
        output_data[2] <= 0;
        output_data[3] <= 0;
    end else begin
        if (ap_start && !processing) begin
            ap_idle <= 0;
            ap_ready <= 0;
            processing <= 1;
        end else if (processing) begin
            // Dummy operation: just sum some inputs for example
            output_data[0] <= input_data[0] + input_data[1];
            output_data[1] <= input_data[2] + input_data[3];
            output_data[2] <= input_data[4] + input_data[5];
            output_data[3] <= input_data[6] + input_data[7];

            ap_done <= 1;
            ap_idle <= 1;
            ap_ready <= 1;
            processing <= 0;
        end else begin
            ap_done <= 0;
        end
    end
end

endmodule
