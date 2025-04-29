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
    output  reg [10:0]  user_data_commit_len, 
    input  wire        user_data_free,        

    // Connection to hls4ml IP
    output wire [3199:0] hls4ml_input_data_flat,
    input  wire [127:0]  hls4ml_output_data_flat,
    output reg           hls4ml_start,
    input  wire          hls4ml_done,

    // debug
    output wire fifo_empty,
    output wire fifo_full
);

// FIFO signals
wire [31:0] fifo_dout;
reg  fifo_rd_en;

// Internal buffers
reg [31:0] hls4ml_input_data_array [0:99];
reg [6:0] read_count;
reg fifo_rd_en_d;

reg [31:0] hls4ml_output_data_array [0:3];
reg [1:0] send_count;
reg sending_result;
reg [1:0]  send_cnt = 2'd0;  // 0-3 result word index
reg waiting_free;
localparam OUT_WORDS = 4; // (128 bit /32)

// FIFO instance (internal)
fifo_generator_0 fifo_inst (
    .clk(clk),
    .srst(rst),
    .din(waveform_data_in),
    .wr_en(waveform_wr_en),
    .rd_en(fifo_rd_en),
    .dout(fifo_dout),
    .full(fifo_full),
    .empty(fifo_empty)
);

// Flatten input array for hls4ml
genvar i;
generate
    for (i = 0; i < 100; i = i + 1) begin : flatten_input
        assign hls4ml_input_data_flat[i*32 +: 32] = hls4ml_input_data_array[i];
    end
endgenerate

// Control Logic
always @(posedge clk) begin
    fifo_rd_en <= 1'b0;
    user_data_write <= 1'b0;
    user_data_commit <= 1'b0;
    user_data_commit_len <= 11'd0;

    if (rst) begin
        fifo_rd_en_d <= 1'b0;
        hls4ml_start <= 1'b0;
        read_count <= 0;
        sending_result <= 1'b0;
        send_count <= 0;
        waiting_free <= 1'b0;
    end else begin

        // --- FIFO Read and hls4ml Input Fill ---
        if (!fifo_empty && read_count < 7'd100) begin
            fifo_rd_en <= 1'b1;
        end

        if (fifo_rd_en_d) begin
            hls4ml_input_data_array[read_count] <= fifo_dout;
            read_count <= read_count + 1;
        end

        if (read_count == 7'd100) begin
            hls4ml_start <= 1'b1;
            read_count <= 0;
        end else begin
            hls4ml_start <= 1'b0;
        end

        fifo_rd_en_d <= fifo_rd_en;

        // --- hls4ml Output Return ---
        if (hls4ml_done && !sending_result) begin
            hls4ml_output_data_array[0] <= hls4ml_output_data_flat[31:0];
            hls4ml_output_data_array[1] <= hls4ml_output_data_flat[63:32];
            hls4ml_output_data_array[2] <= hls4ml_output_data_flat[95:64];
            hls4ml_output_data_array[3] <= hls4ml_output_data_flat[127:96];
            sending_result <= 1'b1;
            waiting_free <= 0; // wait for first free
            send_count <= 0;
        end

        if (sending_result) begin
            /* 2-1) wait until at least one free slot is signalled */
            if (!waiting_free) begin
                if (user_data_free)
                    waiting_free <= 1'b1;
            end
            /* 2-2) transmit the four words sequentially           */
            else begin
                if (send_cnt < OUT_WORDS) begin
                    user_data_word   <= hls4ml_output_data_array[send_cnt];
                    user_data_offset <= {8'd0, send_cnt};
                    user_data_write  <= 1'b1;        // one-clock strobe
                    send_cnt         <= send_cnt + 1'b1;
                    waiting_free     <= 1'b0;        // check free again
                end
                /* 2-3) all words sent → issue commit */
                else begin
                    user_data_commit_len <= OUT_WORDS;  // word count
                    user_data_commit     <= 1'b1;       // one-clock strobe
                    sending_result       <= 1'b0;       // back to idle
                end
            end
        end
    end
end

endmodule

