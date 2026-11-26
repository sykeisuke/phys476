`timescale 1ns / 1ps
`default_nettype none

// 2-bit equality comparator from gates, and the one-line version.
module cmp2 (
    input  wire [1:0] a, b,
    output wire       eq_gates,
    output wire       eq
);
    assign eq_gates = ~(a[1] ^ b[1]) & ~(a[0] ^ b[0]);   // XNOR, XNOR, AND
    assign eq       = (a == b);
endmodule

`default_nettype wire
