`timescale 1ns / 1ps
`default_nettype none

// Full subtractor: d = a - b - bi, with borrow-out bo.
module full_sub (
    input  wire a, b, bi,
    output wire d,      // difference
    output wire bo      // borrow out
);
    assign d  = a ^ b ^ bi;
    assign bo = (~a & b) | (~(a ^ b) & bi);
endmodule

`default_nettype wire
