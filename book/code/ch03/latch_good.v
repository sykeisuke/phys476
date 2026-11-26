`timescale 1ns / 1ps
`default_nettype none

// GOOD: a default assignment at the top means 'q' is assigned on
// every path, so the block is purely combinational.
module latch_good (
    input  wire       en,
    input  wire [7:0] d,
    output reg  [7:0] q
);
    always @(*) begin
        q = 8'd0;        // default
        if (en)
            q = d;
    end
endmodule

`default_nettype wire
