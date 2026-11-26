`timescale 1ns / 1ps
`default_nettype none

// T flip-flop: q toggles at a rising clock edge when t = 1.
module tff (
    input  wire clk,
    input  wire rst,   // synchronous
    input  wire t,
    output reg  q
);
    always @(posedge clk)
        if (rst)
            q <= 1'b0;
        else if (t)
            q <= ~q;
endmodule

`default_nettype wire
