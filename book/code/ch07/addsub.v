`timescale 1ns / 1ps
`default_nettype none

// Signed (two's complement) add/subtract with overflow detection.
// sub = 0: y = a + b,  sub = 1: y = a - b.
module addsub #(
    parameter N = 8
) (
    input  wire signed [N-1:0] a, b,
    input  wire                sub,
    output wire signed [N-1:0] y,
    output wire                ovf     // result does not fit in N bits
);
    wire [N-1:0] bx = sub ? ~b : b;    // a - b = a + ~b + 1
    assign y = a + bx + sub;
    // Overflow: both operands have the same sign, the result has the other.
    assign ovf = (a[N-1] == bx[N-1]) && (y[N-1] != a[N-1]);
endmodule

`default_nettype wire
