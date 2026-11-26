`timescale 1ns / 1ps
`default_nettype none

// Lab 2 test source: a start pulse every PERIOD clocks and a stop pulse
// DELAY clocks after it (both 2 clocks wide). With a 100 MHz clock and
// PERIOD = 100000 the pair repeats every 1 ms and one DELAY step is 10 ns.
// delay_in is sampled once per period, so a switch that moves during a
// measurement cannot give a half-old, half-new delay.
module pulse_pair #(
    parameter PERIOD = 100000
) (
    input  wire       clk,
    input  wire       rst,           // synchronous, active high
    input  wire [7:0] delay_in,      // already synchronized to clk
    output reg        start,
    output reg        stop
);
    reg [31:0] tick;
    reg [7:0]  delay;

    always @(posedge clk) begin
        if (rst) begin
            tick  <= 0;
            delay <= 0;
            start <= 1'b0;
            stop  <= 1'b0;
        end else begin
            if (tick == PERIOD - 1) begin
                tick  <= 0;
                delay <= delay_in;   // new setting for the next pair
            end else begin
                tick <= tick + 1;
            end
            start <= (tick < 2);
            stop  <= (tick >= delay) && (tick < delay + 2);
        end
    end
endmodule

`default_nettype wire
