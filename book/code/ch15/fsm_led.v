`timescale 1ns / 1ps
`default_nettype none

// Exercise 2: four states S0 -> S1 -> S2 -> S3 -> S0, one step per tick.
// led[2:0] = 001, 010, 100, 000. The tick is a clock enable made from the
// 100 MHz clock (DIV = 25_000_000: 4 steps per second); no divided clock.
module fsm_led #(
    parameter DIV = 25_000_000,
    parameter W   = 25
) (
    input  wire       clk,
    input  wire       rst,
    output reg  [2:0] led
);
    localparam [1:0] S0 = 2'd0, S1 = 2'd1, S2 = 2'd2, S3 = 2'd3;

    reg [W-1:0] cnt;
    wire        tick = (cnt == DIV - 1);
    reg  [1:0]  state, next;

    always @(*) begin
        next = state;
        if (tick)
            case (state)
                S0:      next = S1;
                S1:      next = S2;
                S2:      next = S3;
                S3:      next = S0;
                default: next = S0;
            endcase
    end

    always @(posedge clk) begin
        if (rst) begin
            cnt   <= {W{1'b0}};
            state <= S0;
            led   <= 3'b001;
        end else begin
            cnt   <= tick ? {W{1'b0}} : cnt + 1'b1;
            state <= next;
            case (next)                       // Moore output, registered
                S0:      led <= 3'b001;
                S1:      led <= 3'b010;
                S2:      led <= 3'b100;
                default: led <= 3'b000;
            endcase
        end
    end
endmodule

`default_nettype wire
