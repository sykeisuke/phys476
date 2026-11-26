`timescale 1ns / 1ps
`default_nettype none

// Board exercise: one 100 MHz clock, a 1-per-second tick as clock enable,
// a switch shifted into a 4-bit register shown on LEDs.
module sipo_board #(
    parameter DIV = 100_000_000
) (
    input  wire       clk,
    input  wire       cpu_resetn,   // reset button, active low
    input  wire       sw,
    output wire [3:0] led
);
    wire rst, tick;
    reset_sync u_rst (.clk(clk), .arst_n(cpu_resetn), .rst(rst));
    tick_gen #(.DIV(DIV)) u_tick (.clk(clk), .rst(rst), .tick(tick));
    shift_reg4 u_sr (.clk(clk), .rst(rst), .ce(tick), .sin(sw), .q(led));
endmodule

`default_nettype wire
