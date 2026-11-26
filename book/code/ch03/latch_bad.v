`timescale 1ns / 1ps
`default_nettype none

// BAD: when 'en' is 0, 'q' is not assigned, so it must keep its old
// value. Synthesis builds a latch to do that.
module latch_bad (
    input  wire       en,
    input  wire [7:0] d,
    output reg  [7:0] q
);
    always @(*) begin
        if (en)
            q = d;
    end
endmodule

`default_nettype wire
