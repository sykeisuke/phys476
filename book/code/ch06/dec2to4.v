`timescale 1ns / 1ps
`default_nettype none

// 2-to-4 decoder with enable: exactly one output is high when en = 1.
module dec2to4 (
    input  wire [1:0] a,
    input  wire       en,
    output wire [3:0] d
);
    assign d = en ? (4'b0001 << a) : 4'b0000;
endmodule

`default_nettype wire
