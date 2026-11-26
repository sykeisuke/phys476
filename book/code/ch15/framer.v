`timescale 1ns / 1ps
`default_nettype none

// Data framer: on 'start' it sends HEADER, N data words, FOOTER.
// Data source handshake: the source always shows its current word on
// 'din'; the framer raises 'next_word' in each cycle in which it takes
// that word, and the source then moves on to the following word.
// (In a real readout the source is a buffer memory, next chapter.)
// Safe FSM style: state register + next-state block with a default,
// a default branch, registered outputs.
module framer #(
    parameter        N      = 4,          // data words per frame (1..255)
    parameter [15:0] HEADER = 16'hA5A5,
    parameter [15:0] FOOTER = 16'h5A5A
) (
    input  wire        clk,
    input  wire        rst,          // synchronous, active high
    input  wire        start,        // ignored while busy
    input  wire [15:0] din,
    output wire        next_word,    // 1: din is taken this cycle
    output reg  [15:0] dout,
    output reg         dout_valid,
    output wire        busy
);
    localparam [1:0] IDLE = 2'd0, HEAD = 2'd1, DATA = 2'd2, FOOT = 2'd3;

    reg [1:0] state, next;
    reg [7:0] cnt;                    // data words sent in this frame

    // next-state logic
    always @(*) begin
        next = state;                 // default: stay
        case (state)
            IDLE:    if (start) next = HEAD;
            HEAD:    next = DATA;
            DATA:    if (cnt == N - 1) next = FOOT;
            FOOT:    next = IDLE;
            default: next = IDLE;
        endcase
    end

    // state register and registered outputs
    always @(posedge clk) begin
        if (rst) begin
            state      <= IDLE;
            cnt        <= 8'd0;
            dout       <= 16'd0;
            dout_valid <= 1'b0;
        end else begin
            state      <= next;
            dout_valid <= 1'b0;       // default
            case (state)
                HEAD: begin
                    dout       <= HEADER;
                    dout_valid <= 1'b1;
                end
                DATA: begin
                    dout       <= din;
                    dout_valid <= 1'b1;
                    cnt        <= cnt + 8'd1;
                end
                FOOT: begin
                    dout       <= FOOTER;
                    dout_valid <= 1'b1;
                    cnt        <= 8'd0;
                end
                default: ;
            endcase
        end
    end

    assign next_word = (state == DATA);   // decoded from the state only
    assign busy  = (state != IDLE);
endmodule

`default_nettype wire
