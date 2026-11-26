`timescale 1ns / 1ps
`default_nettype none

// Rising-edge detector: 'rise' is 1 for exactly one clock cycle
// after 'in' goes from 0 to 1. 'in' must be synchronous to clk.
module edge_detect (
    input  wire clk,
    input  wire in,
    output wire rise
);
    reg in_d = 1'b0;

    always @(posedge clk)
        in_d <= in;

    assign rise = in & ~in_d;
endmodule

`default_nettype wire
