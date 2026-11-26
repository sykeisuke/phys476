`timescale 1ns / 1ps
`default_nettype none

// Calorimeter trigger: trig = 1 when e0 + e1 + e2 + e3 > th.
module calo_trigger (
    input  wire [7:0] e0, e1, e2, e3,
    input  wire [9:0] th,
    output wire [9:0] esum,
    output wire       trig
);
    add_sum4 u_add (.e0(e0), .e1(e1), .e2(e2), .e3(e3), .sum(esum));
    comparator_gt u_cmp (.a(esum), .th(th), .gt(trig));
endmodule

`default_nettype wire
