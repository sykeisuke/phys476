`timescale 1ns / 1ps
`default_nettype none

// The seven basic gates, applied bit by bit to two 1-bit inputs.
module gates (
    input  wire a, b,
    output wire y_not, y_and, y_or, y_xor,
    output wire y_nand, y_nor, y_xnor
);
    assign y_not  = ~a;
    assign y_and  = a & b;
    assign y_or   = a | b;
    assign y_xor  = a ^ b;
    assign y_nand = ~(a & b);
    assign y_nor  = ~(a | b);
    assign y_xnor = ~(a ^ b);
endmodule

`default_nettype wire
