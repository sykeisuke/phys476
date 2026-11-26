`timescale 1ns / 1ps
`default_nettype none

// 3-bit subtractor with a sign-magnitude output for LEDs.
// diff is the 4-bit two's complement result (-7 .. +7).
// For display, a negative result is negated (invert, add 1) to get its magnitude.
module sub3_signmag (
    input  wire [2:0] a, b,
    output wire [3:0] diff,   // two's complement
    output wire       neg,    // sign: 1 = negative
    output wire [2:0] mag     // magnitude |a - b|
);
    assign diff = {1'b0, a} - {1'b0, b};
    assign neg  = diff[3];
    assign mag  = neg ? (~diff[2:0] + 3'd1) : diff[2:0];
endmodule

`default_nettype wire
