`timescale 1ns / 1ps
`default_nettype none

// Board top for the Nexys Video: a = SW[3:0], b = SW[7:4], {cout, s} on LED[4:0].
// Use with book/code/ch05/nexys_video.xdc (ports sw[7:0], led[7:0]).
module top_adder4 (
    input  wire [7:0] sw,
    output wire [7:0] led
);
    rca #(.N(4)) u_add (.a(sw[3:0]), .b(sw[7:4]), .cin(1'b0),
                        .s(led[3:0]), .cout(led[4]));
    assign led[7:5] = 3'b000;
endmodule

`default_nettype wire
