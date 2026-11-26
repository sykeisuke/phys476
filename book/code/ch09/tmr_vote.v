`timescale 1ns / 1ps
`default_nettype none

// Triple modular redundancy: bitwise 2-out-of-3 majority of three copies.
// One corrupted copy is outvoted; err flags any disagreement.
module tmr_vote #(parameter W = 8) (
    input  wire [W-1:0] a, b, c,
    output wire [W-1:0] y,
    output wire         err
);
    assign y   = (a & b) | (a & c) | (b & c);
    assign err = (a != b) || (a != c);
endmodule

`default_nettype wire
