`timescale 1ns / 1ps
`default_nettype none

// 4-bit shift register: serial in (sin), parallel out (q).
// The first bit received ends up in q[3].
module shift_reg4 (
    input  wire       clk,
    input  wire       rst,   // synchronous
    input  wire       ce,    // shift only when ce = 1
    input  wire       sin,
    output reg  [3:0] q
);
    always @(posedge clk)
        if (rst)
            q <= 4'd0;
        else if (ce)
            q <= {q[2:0], sin};
endmodule

`default_nettype wire
