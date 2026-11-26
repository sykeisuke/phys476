`timescale 1ns / 1ps
`default_nettype none

// Lab 2 top for the Nexys Video: the switches set a start-stop delay
// in 10 ns steps, pulse_pair makes the pulses, coarse_tdc measures them
// (internal connection, no wire), and the LEDs show the last count.
// Expected: SW = 25 (0001_1001) -> LED = 0001_1001. SW = 0 -> LED = 0
// (start and stop coincide, the TDC times out).
module tdc_board_top #(
    parameter PERIOD = 100000,       // 1 ms at 100 MHz
    parameter TMAX   = 2000          // TDC timeout: 20 us
) (
    input  wire       clk,           // 100 MHz, pin R4
    input  wire       cpu_resetn,    // CPU RESET button, active low
    input  wire [7:0] sw,
    output reg  [7:0] led
);
    // reset and switches into the clock domain (2-FF, Chapter 14)
    (* ASYNC_REG = "TRUE" *) reg [1:0] rst_s  = 2'b11;
    (* ASYNC_REG = "TRUE" *) reg [7:0] sw_s1  = 8'd0;
    (* ASYNC_REG = "TRUE" *) reg [7:0] sw_s2  = 8'd0;
    always @(posedge clk) begin
        rst_s <= {rst_s[0], ~cpu_resetn};
        sw_s1 <= sw;
        sw_s2 <= sw_s1;
    end
    wire rst = rst_s[1];

    wire start, stop;
    pulse_pair #(.PERIOD(PERIOD)) u_pulses (
        .clk(clk), .rst(rst), .delay_in(sw_s2), .start(start), .stop(stop));

    wire [15:0] tdc_count;
    wire        valid, timeout;
    coarse_tdc #(.W(16), .TMAX(TMAX)) u_tdc (
        .clk(clk), .rst(rst), .start_in(start), .stop_in(stop),
        .tdc_count(tdc_count), .valid(valid), .timeout(timeout));

    always @(posedge clk) begin
        if (rst)          led <= 8'd0;
        else if (valid)   led <= tdc_count[7:0];
        else if (timeout) led <= 8'd0;
    end
endmodule

`default_nettype wire
