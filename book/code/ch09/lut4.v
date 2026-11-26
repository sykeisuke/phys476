`timescale 1ns / 1ps
`default_nettype none

// A 4-input look-up table: 16 configuration bits and a multiplexer.
// INIT[k] is the output for the input pattern k = {d, c, b, a}.
// 16'hF888 makes y = (a & b) | (c & d).
module lut4 #(parameter [15:0] INIT = 16'hF888) (
    input  wire a, b, c, d,
    output wire y
);
    assign y = INIT[{d, c, b, a}];
endmodule

`default_nettype wire
