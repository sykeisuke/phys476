`timescale 1ns / 1ps
`default_nettype none

// Moore FSM: 'go' starts a 4-bit count from 0 to 15; then 'done' is 1
// until 'go' is released. 'done' depends only on the state and is
// stored in a flip-flop (computed from 'next'), so it cannot glitch.
module fsm_moore (
    input  wire       clk,
    input  wire       rst,           // synchronous, active high
    input  wire       go,
    output reg  [3:0] count,
    output reg        done
);
    localparam [1:0] IDLE = 2'd0, COUNTING = 2'd1, DONE = 2'd2;

    reg [1:0] state, next;

    // next-state logic
    always @(*) begin
        next = state;                         // default: stay
        case (state)
            IDLE:     if (go)             next = COUNTING;
            COUNTING: if (count == 4'hF)  next = DONE;
            DONE:     if (!go)            next = IDLE;
            default:                      next = IDLE;
        endcase
    end

    // state register, datapath (counter) and registered output
    always @(posedge clk) begin
        if (rst) begin
            state <= IDLE;
            count <= 4'd0;
            done  <= 1'b0;
        end else begin
            state <= next;
            count <= (state == COUNTING) ? count + 4'd1 : 4'd0;
            done  <= (next == DONE);
        end
    end
endmodule

`default_nettype wire
