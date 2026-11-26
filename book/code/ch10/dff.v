`timescale 1ns / 1ps
`default_nettype none

// D flip-flop: q takes the value of d at each rising edge of clk.
module dff (
    input  wire clk,
    input  wire d,
    output reg  q
);
    always @(posedge clk)
        q <= d;
endmodule

`default_nettype wire
