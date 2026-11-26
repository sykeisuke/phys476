`timescale 1ns / 1ps
`default_nettype none

// Reset synchronizer: reset is asserted at once (asynchronously) and
// released on a clock edge of this domain. Use one per clock domain.
module reset_sync (
    input  wire clk,
    input  wire arst_n,   // asynchronous reset in, active low
    output wire rst       // reset out, active high, released on a clk edge
);
    (* ASYNC_REG = "TRUE" *)
    reg [1:0] sync = 2'b11;
    always @(posedge clk or negedge arst_n)
        if (!arst_n)
            sync <= 2'b11;
        else
            sync <= {sync[0], 1'b0};
    assign rst = sync[1];
endmodule

`default_nettype wire
