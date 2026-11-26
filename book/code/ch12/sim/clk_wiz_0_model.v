`timescale 1ns / 1ps
`default_nettype none

// SIMULATION-ONLY stand-in for the Clocking Wizard IP clk_wiz_0, for Icarus.
// It passes the clock through (no frequency change) and raises locked after
// LOCK_CYCLES input clocks. Do NOT add this file to a Vivado project that
// contains the generated clk_wiz_0 IP: two modules would have the same name.
module clk_wiz_0 #(
    parameter LOCK_CYCLES = 20
) (
    input  wire clk_in1,
    input  wire reset,      // active high, like the IP
    output wire clk_out1,
    output reg  locked
);
    integer n;
    assign clk_out1 = clk_in1;
    always @(posedge clk_in1 or posedge reset)
        if (reset) begin
            n = 0;
            locked <= 1'b0;
        end else begin
            if (n < LOCK_CYCLES) n = n + 1;
            locked <= (n >= LOCK_CYCLES);
        end
endmodule

`default_nettype wire
