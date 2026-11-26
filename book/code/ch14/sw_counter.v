`timescale 1ns / 1ps
`default_nettype none

// Switch counter for the Nexys Video: each press of BTNC adds 1 to the
// binary number on LED[7:0]. Chain: synchronizer -> debouncer -> edge
// detector -> counter. One clock (100 MHz); the 1 ms tick is an enable.
module sw_counter #(
    parameter DIV = 100_000,      // 100 MHz / 100_000 = 1 kHz tick
    parameter W   = 17,
    parameter K   = 8             // 8 equal samples = 8 ms
) (
    input  wire       clk,        // 100 MHz
    input  wire       cpu_resetn, // reset button, active low
    input  wire       btnc,       // push button, asynchronous
    output reg  [7:0] led
);
    wire rst, btn_s, tick, btn_db, press;

    sync_2ff u_rsync (.clk(clk), .d_async(~cpu_resetn), .q(rst));
    sync_2ff u_bsync (.clk(clk), .d_async(btnc), .q(btn_s));
    tick_gen #(.DIV(DIV), .W(W)) u_tick (.clk(clk), .rst(rst), .tick(tick));
    debounce #(.K(K)) u_db (.clk(clk), .rst(rst), .tick(tick),
                            .in(btn_s), .out(btn_db));
    edge_detect u_edge (.clk(clk), .in(btn_db), .rise(press));

    always @(posedge clk) begin
        if (rst)
            led <= 8'd0;
        else if (press)
            led <= led + 1'b1;
    end
endmodule

`default_nettype wire
