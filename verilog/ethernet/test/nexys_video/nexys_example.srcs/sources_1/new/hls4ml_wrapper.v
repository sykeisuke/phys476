module hls4ml_wrapper (
    input  wire        clk,
    input  wire        rst,

    // Input side (from fakernet_top)
    input  wire [31:0] waveform_data_in,
    input  wire        waveform_wr_en,

    // Output side (to fakernet_top)
    output reg  [31:0] user_data_word,
    output reg  [9:0]  user_data_offset,
    output reg         user_data_write,
    output reg         user_data_commit,
    input  wire [10:0] user_data_commit_len, // (can be unused)
    input  wire        user_data_free,        // (can be unused)

    // Connection to hls4ml IP
    output wire [3199:0] hls4ml_input_data_flat,
    input  wire [127:0]  hls4ml_output_data_flat,
    output reg           hls4ml_start,
    input  wire          hls4ml_done
);

// FIFO signals
wire [31:0] fifo_dout;
reg  fifo_rd_en;
wire fifo_empty;

// Internal buffers
reg [31:0] hls4ml_input_data_array [0:99];
reg [6:0] read_count;
reg fifo_rd_en_d;

reg [31:0] hls4ml_output_data_array [0:3];
reg [1:0] send_count;
reg sending_result;

// FIFO instance (internal)
fifo_generator_0 fifo_inst (
    .clk(clk),
    .srst(rst),
    .din(waveform_data_in),
    .wr_en(waveform_wr_en),
    .rd_en(fifo_rd_en),
    .dout(fifo_dout),
    .full(),
    .empty(fifo_empty)
);

// Flatten input array for hls4ml

// Unroll
// Note: Generate block must be outside always block

genvar i;
generate
    for (i = 0; i < 100; i = i + 1) begin : flatten_input
        assign hls4ml_input_data_flat[i*32 +: 32] = hls4ml_input_data_array[i];
    end
endgenerate

// Control Logic
always @(posedge clk) begin
    fifo_rd_en <= 1'b0;

    if (rst) begin
        fifo_rd_en_d <= 1'b0;
        hls4ml_start <= 1'b0;
        read_count <= 0;
        sending_result <= 1'b0;
        send_count <= 0;
        user_data_write <= 1'b0;
        user_data_commit <= 1'b0;
    end else begin

        // --- FIFO Read and hls4ml Input Fill ---
        if (!fifo_empty && read_count < 100) begin
            fifo_rd_en <= 1'b1;
        end

        if (fifo_rd_en_d) begin
            hls4ml_input_data_array[read_count] <= fifo_dout;
            read_count <= read_count + 1;
        end

        if (read_count == 100) begin
            hls4ml_start <= 1'b1;
            read_count <= 0;
        end else begin
            hls4ml_start <= 1'b0;
        end

        fifo_rd_en_d <= fifo_rd_en;

        // --- hls4ml Output Return ---
        if (hls4ml_done && !sending_result) begin
            sending_result <= 1'b1;
            send_count <= 0;

            hls4ml_output_data_array[0] <= hls4ml_output_data_flat[31:0];
            hls4ml_output_data_array[1] <= hls4ml_output_data_flat[63:32];
            hls4ml_output_data_array[2] <= hls4ml_output_data_flat[95:64];
            hls4ml_output_data_array[3] <= hls4ml_output_data_flat[127:96];
        end

        if (sending_result) begin
            if (send_count < 4) begin
                user_data_word <= hls4ml_output_data_array[send_count];
                user_data_offset <= send_count;
                user_data_write <= 1'b1;
                user_data_commit <= 1'b0;
                send_count <= send_count + 1;
            end else begin
                user_data_write <= 1'b0;
                user_data_commit <= 1'b1;
                sending_result <= 1'b0;
            end
        end else begin
            user_data_write <= 1'b0;
            user_data_commit <= 1'b0;
        end
    end
end

endmodule

