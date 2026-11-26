`timescale 1ns / 1ps
`default_nettype none

// D latch: transparent while en = 1, holds while en = 0.
// (Written on purpose. In normal designs a latch is a bug, see Chapter 3.)
module d_latch (
    input  wire en,
    input  wire d,
    output reg  q
);
    always @(*)
        if (en)
            q = d;
endmodule

`default_nettype wire
