`timescale 1ns / 1ps
`default_nettype none

// gt = 1 when a is strictly greater than th.
module comparator_gt (
    input  wire [9:0] a,
    input  wire [9:0] th,
    output wire       gt
);
    assign gt = (a > th);
endmodule

`default_nettype wire
