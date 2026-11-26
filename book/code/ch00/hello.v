// hello.v -- the smallest useful module: a 2-input AND gate.
// Chapter 0 checkpoint (same design as book/appendix-setup.qmd, Section 5).
`timescale 1ns / 1ps
`default_nettype none

module hello (
    input  wire a,
    input  wire b,
    output wire y
);
    assign y = a & b;
endmodule

`default_nettype wire
