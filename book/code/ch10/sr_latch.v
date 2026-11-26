`timescale 1ns / 1ps
`default_nettype none

// SR latch from two cross-coupled NOR gates (simulation model).
// The #1 gate delays are for simulation only; synthesis ignores them.
module sr_latch (
    input  wire s,
    input  wire r,
    output wire q,
    output wire qn
);
    assign #1 q  = ~(r | qn);
    assign #1 qn = ~(s | q);
endmodule

`default_nettype wire
