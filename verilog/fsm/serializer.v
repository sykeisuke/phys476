module serializer (
    input wire clk,
    input wire rst,
    input wire [7:0] din,
    output reg [7:0] dout
);

    // Define HEADER and FOOTER as parameters
    parameter [7:0] HEADER = 8'hAA;
    parameter [7:0] FOOTER = 8'hFF;
    parameter integer NUM_CHANNELS = 16;
    parameter integer TIMEOUT = 20; // wait for 20 clk cycles before sending footer

    // State encoding using parameter
    parameter [1:0] IDLE = 2'b00, 
                    SEND_HEADER = 2'b01, 
                    SEND_DATA = 2'b10, 
                    SEND_FOOTER = 2'b11;

    reg [1:0] state, next_state;
    reg [3:0] channel_counter;
    reg [4:0] timeout_counter; // timer for footer sendout

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
        case (state)
            IDLE: 
                next_state = SEND_HEADER;
            SEND_HEADER: 
                next_state = SEND_DATA;
            SEND_DATA: 
                if (channel_counter == (NUM_CHANNELS - 1) || timeout_counter == TIMEOUT) begin
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
            timeout_counter <= 0;
        end else if (state == SEND_DATA) begin
            channel_counter <= channel_counter + 1;
            timeout_counter <= 0; 
        end else if (state == SEND_FOOTER) begin
            channel_counter <= 0;
            timeout_counter <= 0;
        end else begin
            timeout_counter <= timeout_counter + 1;
        end
    end

    // Output logic (synchronous reset)
    always @(posedge clk) begin
        if (rst) begin
            dout <= 8'b0;
        end else begin
            case (state)
                SEND_HEADER: dout <= HEADER;
                SEND_DATA: dout <= din;
                SEND_FOOTER: dout <= FOOTER;
                default: dout <= 8'b0;
            endcase
        end
    end

endmodule

