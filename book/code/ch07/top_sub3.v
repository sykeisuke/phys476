`timescale 1ns / 1ps
`default_nettype none

// Board tops for the two optional 2026 board labs (Lec10 exercises 1 and 2), Nexys Video.
// a = SW[2:0], b = SW[5:3]; SW[7:6] are not used; LED[7:4] are off.
// Use with book/code/ch05/nexys_video.xdc (ports sw[7:0], led[7:0]).

// Exercise A: 3-bit subtractor. LED[3:0] = {BO, D2, D1, D0} = a - b in 4-bit two's complement.
module top_sub3 (
    input  wire [7:0] sw,
    output wire [7:0] led
);
    wire       neg;
    wire [2:0] mag;
    sub3_signmag u_sub (.a(sw[2:0]), .b(sw[5:3]), .diff(led[3:0]), .neg(neg), .mag(mag));
    assign led[7:4] = 4'b0000;
endmodule

// Exercise B: the same subtraction shown in sign-magnitude form.
// LED3 = sign (1 = negative), LED[2:0] = |a - b|.
module top_sub3_signmag (
    input  wire [7:0] sw,
    output wire [7:0] led
);
    wire [3:0] diff;
    sub3_signmag u_sub (.a(sw[2:0]), .b(sw[5:3]), .diff(diff), .neg(led[3]), .mag(led[2:0]));
    assign led[7:4] = 4'b0000;
endmodule

`default_nettype wire
