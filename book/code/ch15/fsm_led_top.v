`timescale 1ns / 1ps
`default_nettype none

// Board top for the LED pattern FSM on the Nexys Video.
// LED[2:0] show 001, 010, 100, 000, one step every 0.25 s; LED[7:3] stay off.
// Needs sync_2ff.v from book/code/ch14 (reset button synchronizer).
module fsm_led_top #(
    parameter DIV = 25_000_000,   // 0.25 s at 100 MHz
    parameter W   = 25
) (
    input  wire       clk,        // 100 MHz
    input  wire       cpu_resetn, // reset button, active low
    output wire [7:0] led
);
    wire rst;
    sync_2ff u_rsync (.clk(clk), .d_async(~cpu_resetn), .q(rst));

    fsm_led #(.DIV(DIV), .W(W)) u_fsm (.clk(clk), .rst(rst), .led(led[2:0]));
    assign led[7:3] = 5'b00000;
endmodule

`default_nettype wire
