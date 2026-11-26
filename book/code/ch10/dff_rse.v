`timescale 1ns / 1ps
`default_nettype none

// D flip-flop with synchronous reset, set and clock enable.
// Priority: reset, then set, then clock enable.
module dff_rse (
    input  wire clk,
    input  wire rst,
    input  wire set,
    input  wire ce,
    input  wire d,
    output reg  q
);
    always @(posedge clk)
        if (rst)
            q <= 1'b0;
        else if (set)
            q <= 1'b1;
        else if (ce)
            q <= d;
endmodule

`default_nettype wire
