`timescale 1ns / 1ps
`default_nettype none

// Testbench for tdc_board_top (Lab 2): sets the switches and checks the
// LEDs. PERIOD is shortened to 600 clocks (6 us) so it runs fast; on the
// board it is 100000 (1 ms). TMAX = 500 so that SW = 0 times out.
//   iverilog -g2005 -Wall -o tb_board tb_tdc_board.v tdc_board_top.v pulse_pair.v coarse_tdc.v
//   vvp tb_board
module tb_tdc_board;
    reg clk = 0;
    always #5 clk = ~clk;            // 100 MHz

    reg        cpu_resetn = 0;
    reg  [7:0] sw = 0;
    wire [7:0] led;
    tdc_board_top #(.PERIOD(600), .TMAX(500)) dut (
        .clk(clk), .cpu_resetn(cpu_resetn), .sw(sw), .led(led));

    integer i, errors = 0;
    reg [7:0] settings [0:5];
    initial begin
        settings[0] = 8'd25;  settings[1] = 8'd1;   settings[2] = 8'd2;
        settings[3] = 8'd100; settings[4] = 8'd255; settings[5] = 8'd0;
        repeat (5) @(posedge clk);
        cpu_resetn = 1;
        for (i = 0; i < 6; i = i + 1) begin
            sw = settings[i];
            repeat (3 * 600) @(posedge clk);   // 3 periods: new delay is in use
            $display("SW = %3d -> LED = %3d (%b)", sw, led, led);
            if (led !== sw) errors = errors + 1;
        end
        if (errors == 0) $display("PASS (LED = SW for 6 settings)");
        else             $display("FAIL (%0d settings wrong)", errors);
        $finish;
    end
endmodule

`default_nettype wire
