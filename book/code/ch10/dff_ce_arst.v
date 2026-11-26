`timescale 1ns / 1ps
`default_nettype none

// D flip-flop with clock enable and asynchronous clear (maps to FDCE).
module dff_ce_arst (
    input  wire clk,
    input  wire clr,   // asynchronous, active high
    input  wire ce,
    input  wire d,
    output reg  q
);
    always @(posedge clk or posedge clr)
        if (clr)
            q <= 1'b0;
        else if (ce)
            q <= d;
endmodule

`default_nettype wire
