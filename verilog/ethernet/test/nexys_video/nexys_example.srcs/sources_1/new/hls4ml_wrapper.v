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
    output reg [10:0]  user_data_commit_len, 
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
reg [31:0] hls4ml_input_data_array [0:99];

genvar i;

generate
    for (i = 0; i < 100; i = i + 1) begin : flatten_input
        assign hls4ml_input_data_flat[i*32 +: 32] = hls4ml_input_data_array[i];
    end
endgenerate

// FIFO reading and hls4ml start
reg [6:0] read_count;
reg fifo_rd_en_d;

always @(posedge clk) begin
    fifo_rd_en <= 1'b0;
    hls4ml_start <= 1'b0;

    if (rst) begin
        fifo_rd_en_d <= 1'b0;
        read_count   <= 0;
    end else begin
        if (!fifo_empty && read_count < 7'd100) begin
            fifo_rd_en <= 1'b1;
        end

        if (fifo_rd_en_d) begin
            hls4ml_input_data_array[read_count] <= fifo_dout;
            read_count <= read_count + 1;
        end

        if (read_count == 7'd100) begin
            hls4ml_start <= 1'b1;
            read_count   <= 0;
        end

        fifo_rd_en_d <= fifo_rd_en;
    end
end

// hls4ml output and sending out data
localparam OUT_WORDS = 4; // (128 bit /32)

reg [31:0] hls4ml_output_data_array [0:3];
reg [1:0] send_count = 2'd0; // 0-3 result word index
reg sending_result;
reg waiting_free;
reg commit_pending;
reg [10:0] commit_len_reg;

always @(posedge clk) begin
    user_data_write      <= 1'b0;
    user_data_commit     <= 1'b0;
    user_data_commit_len <= 11'd0;

    if (rst) begin
        sending_result <= 1'b0;
        waiting_free   <= 1'b0;
        send_count     <= 1'b0;
        commit_pending <= 1'b0;
        commit_len_reg <= 1'b0;
    end else begin
        if (hls4ml_done && !sending_result) begin
            hls4ml_output_data_array[0] <= hls4ml_output_data_flat[31:0];
            hls4ml_output_data_array[1] <= hls4ml_output_data_flat[63:32];
            hls4ml_output_data_array[2] <= hls4ml_output_data_flat[95:64];
            hls4ml_output_data_array[3] <= hls4ml_output_data_flat[127:96];
            sending_result <= 1'b1;
            waiting_free   <= 1'b0;
            send_count     <= 0;
        end

        if (sending_result) begin
            /* wait until at least one free slot is signalled */
            if (!waiting_free) begin
                if (user_data_free)
                    waiting_free <= 1'b1;
            end
            /* transmit the four words sequentially */
            else begin
                if (send_count < OUT_WORDS) begin
                    user_data_word   <= hls4ml_output_data_array[send_count];
                    user_data_offset <= {8'd0, send_count};
                    user_data_write  <= 1'b1;
                    send_count       <= send_count + 1;
                    waiting_free     <= 1'b0; // wait for next free
                end 
                else begin
                    commit_pending <= 1'b1;
                    commit_len_reg <= OUT_WORDS;
                    sending_result <= 1'b0;
                    end
                end
            end
        end

        /* all words sent → issue commit */
        if (commit_pending) begin
            user_data_commit     <= 1'b1;
            user_data_commit_len <= commit_len_reg;
            commit_pending       <= 1'b0;
        end
    end
end

endmodule

