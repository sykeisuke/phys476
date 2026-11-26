`timescale 1ns / 1ps
`default_nettype none

// NOT, AND and OR built only from NAND, as in the figure.
module nand_only (
    input  wire a, b,
    output wire y_not, y_and, y_or
);
    wire n_ab = ~(a & b);          // NAND(a, b)
    assign y_not = ~(a & a);       // NAND with tied inputs
    assign y_and = ~(n_ab & n_ab); // invert NAND -> AND
    wire na = ~(a & a);
    wire nb = ~(b & b);
    assign y_or  = ~(na & nb);     // De Morgan: NAND(~a, ~b) = a | b
endmodule

`default_nettype wire
