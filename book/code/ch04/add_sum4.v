`timescale 1ns / 1ps
`default_nettype none

// Sum of four 8-bit energy deposits. 4 * 255 = 1020 needs 10 bits.
module add_sum4 (
    input  wire [7:0] e0, e1, e2, e3,
    output wire [9:0] sum
);
    assign sum = e0 + e1 + e2 + e3;
endmodule

`default_nettype wire
