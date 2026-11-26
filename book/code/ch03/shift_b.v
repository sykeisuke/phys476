`timescale 1ns / 1ps
`default_nettype none

// WRONG: the same code with blocking (=). Each line sees the value
// written by the line above it, so din reaches q3 in one clock.
module shift_b (
    input  wire clk,
    input  wire din,
    output reg  q1, q2, q3
);
    always @(posedge clk) begin
        q1 = din;
        q2 = q1;
        q3 = q2;
    end
endmodule

`default_nettype wire
