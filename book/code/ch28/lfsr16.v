`timescale 1ns / 1ps
`default_nettype none

// 16-bit Fibonacci LFSR (linear-feedback shift register): a shift register whose new
// input bit is the XOR of a few of its own bits (taps 16, 14, 13, 11). With these taps
// it steps through all 65535 non-zero states before repeating: a cheap pseudo-random
// number generator. The all-zero state is forbidden (it would stay 0), so reset loads 1.
// Pulse generator: fire when q < THRESH, i.e. with probability about THRESH / 65536
// per enabled clock.
module lfsr16 (
    input  wire        clk,
    input  wire        rst,
    input  wire        en,
    output reg  [15:0] q
);
    wire fb = q[15] ^ q[13] ^ q[12] ^ q[10];

    always @(posedge clk) begin
        if (rst)
            q <= 16'h0001;
        else if (en)
            q <= {q[14:0], fb};
    end
endmodule
