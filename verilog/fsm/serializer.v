module serializer (
    input wire clk,
    input wire rst,
    input wire [7:0] din,
    input wire din_valid,
    output reg [7:0] dout,
    output reg dout_valid
);

    // Define HEADER and FOOTER as parameters
    parameter [7:0] HEADER = 8'hAA;
    parameter [7:0] FOOTER = 8'hFF;
    parameter integer NUM_CHANNELS = 16;

    // State encoding using parameter
    parameter [1:0] IDLE = 2'b00, 
                    SEND_HEADER = 2'b01, 
                    SEND_DATA = 2'b10, 
                    SEND_FOOTER = 2'b11;

    reg [1:0] state, next_state;
    reg [3:0] channel_counter;
    reg [7:0] current_data;
    reg data_ready;

    // State register (synchronous reset)
    always @(posedge clk) begin
        if (rst) begin
            state <= IDLE;
        end else begin
            state <= next_state;
        end
    end

    // Next state logic
    always @(*) begin
        next_state = state;
        data_ready = 1'b0;
        case (state)
            IDLE: 
                if (din_valid) begin
                    next_state = SEND_HEADER;
                end
            SEND_HEADER: 
                next_state = SEND_DATA;
            SEND_DATA: 
                if (channel_counter == (NUM_CHANNELS-1)) begin
                    next_state = SEND_FOOTER;
                end
            SEND_FOOTER: 
                next_state = IDLE;
            default: 
                next_state = IDLE;
        endcase
    end

    // Channel counter logic (synchronous reset)
    always @(posedge clk) begin
        if (rst) begin
            channel_counter <= 0;
        end else if (state == SEND_DATA && din_valid) begin
            channel_counter <= channel_counter + 1;
        end else if (state == SEND_FOOTER) begin
            channel_counter <= 0;
        end
    end

    // Output logic (synchronous reset)
    always @(posedge clk) begin
        if (rst) begin
            dout <= 8'b0;
            dout_valid <= 1'b0;
        end else begin
            case (state)
                SEND_HEADER: begin
                    dout <= HEADER;
                    dout_valid <= 1'b1;
                end
                SEND_DATA: begin
                    dout <= din;
                    dout_valid <= 1'b1;
                end
                SEND_FOOTER: begin
                    dout <= FOOTER;
                    dout_valid <= 1'b1;
                end
                default: begin
                    dout <= 8'b0;
                    dout_valid <= 1'b0;
                end
            endcase
        end
    end

endmodule

