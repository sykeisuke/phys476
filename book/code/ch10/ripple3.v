`timescale 1ns / 1ps
`default_nettype none

// 3-bit ripple counter: each stage is clocked by the stage before it.
// For understanding only: in an FPGA use a synchronous counter (Chapter 11).
module ripple3 (
    input  wire       clk,
    input  wire       clr,   // asynchronous clear
    output reg  [2:0] q
);
    always @(negedge clk or posedge clr)
        if (clr) q[0] <= 1'b0; else q[0] <= ~q[0];
    always @(negedge q[0] or posedge clr)
        if (clr) q[1] <= 1'b0; else q[1] <= ~q[1];
    always @(negedge q[1] or posedge clr)
        if (clr) q[2] <= 1'b0; else q[2] <= ~q[2];
endmodule

`default_nettype wire
