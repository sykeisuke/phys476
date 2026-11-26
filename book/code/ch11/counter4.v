`timescale 1ns / 1ps
`default_nettype none

// 4-bit binary counter with clock enable, terminal count and
// clock enable out (CEO) for cascading.
module counter4 (
    input  wire       clk,
    input  wire       rst,   // synchronous
    input  wire       ce,
    output reg  [3:0] q,
    output wire       tc,    // terminal count: q = 1111
    output wire       ceo    // clock enable out: drives the next CE
);
    always @(posedge clk)
        if (rst)
            q <= 4'd0;
        else if (ce)
            q <= q + 4'd1;

    assign tc  = &q;
    assign ceo = ce & tc;
endmodule

`default_nettype wire
