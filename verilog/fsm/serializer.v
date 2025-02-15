module serializer (
    input wire clk,
    input wire rst_n,
    input wire [7:0] din,
    input wire din_valid,
    output reg [7:0] dout,
    output reg dout_valid
);

localparam HEADER = 8'hAA;
localparam FOOTER = 8'hFF;
localparam NUM_CHANNELS = 4;

typedef enum logic [1:0] {IDLE, SEND_HEADER, SEND_DATA, SEND_FOOTER} state_t;
state_t state, next_state;

reg [3:0] channel_counter;
reg [7:0] current_data;
reg data_ready;

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        state <= IDLE;
    end else begin
        state <= next_state;
    end
end

always @(*) begin
    next_state = state;
    data_ready = 1'b0;
    case (state)
        IDLE: if (din_valid) next_state = SEND_HEADER;
        SEND_HEADER: next_state = SEND_DATA;
        SEND_DATA: if (channel_counter == NUM_CHANNELS - 1) next_state = SEND_FOOTER;
        SEND_FOOTER: next_state = IDLE;
    endcase
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        channel_counter <= 0;
    end else if (state == SEND_DATA && din_valid) begin
        channel_counter <= channel_counter + 1;
    end else if (state == SEND_FOOTER) begin
        channel_counter <= 0;
    end
end
endmodule
