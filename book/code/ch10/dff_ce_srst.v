`timescale 1ns / 1ps
`default_nettype none

// D flip-flop with clock enable and synchronous reset (maps to FDRE).
module dff_ce_srst (
    input  wire clk,
    input  wire rst,   // synchronous, active high, wins over ce
    input  wire ce,
    input  wire d,
    output reg  q
);
    always @(posedge clk)
        if (rst)
            q <= 1'b0;
        else if (ce)
            q <= d;
endmodule

`default_nettype wire
