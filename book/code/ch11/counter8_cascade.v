`timescale 1ns / 1ps
`default_nettype none

// 8-bit counter from two 4-bit counters. Both use the same clock;
// the upper counter is enabled by the CEO of the lower one.
module counter8_cascade (
    input  wire       clk,
    input  wire       rst,
    input  wire       ce,
    output wire [7:0] q,
    output wire       ceo
);
    wire ceo_lo;
    counter4 u_lo (.clk(clk), .rst(rst), .ce(ce),     .q(q[3:0]), .tc(), .ceo(ceo_lo));
    counter4 u_hi (.clk(clk), .rst(rst), .ce(ceo_lo), .q(q[7:4]), .tc(), .ceo(ceo));
endmodule

`default_nettype wire
