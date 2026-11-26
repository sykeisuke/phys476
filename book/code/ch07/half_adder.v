`timescale 1ns / 1ps
`default_nettype none

// Half adder: adds two bits.
module half_adder (
    input  wire a, b,
    output wire s,      // sum
    output wire c       // carry
);
    assign s = a ^ b;
    assign c = a & b;
endmodule

`default_nettype wire
