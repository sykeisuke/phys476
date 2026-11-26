`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for counter32_top (core labs of Chapter 12).
// Sources: tb_ch12.v counter32_top.v reset_sync.v
module tb_ch12;
    integer errors = 0;

    task check(input cond, input [8*24-1:0] what);
        if (!cond) begin
            $error("%0s", what);
            errors = errors + 1;
        end
    endtask

    reg clk = 1'b0;
    always #5 clk = ~clk;               // 100 MHz

    reg cpu_resetn = 1'b0;              // button pressed at power-up
    wire [7:0] led;
    counter32_top dut (.clk(clk), .cpu_resetn(cpu_resetn), .led(led));

    initial begin
        repeat (3) @(posedge clk);
        #1 check(dut.rst === 1'b1 && dut.cnt === 32'd0, "held in reset");
        cpu_resetn = 1'b1;              // release the button between edges
        @(posedge clk); #1 check(dut.rst === 1'b1, "still reset, 1st edge");
        @(posedge clk); #1 check(dut.rst === 1'b0, "released on 2nd edge");
        check(dut.cnt === 32'd0, "starts from 0");
        repeat (50) @(posedge clk);
        #1 check(dut.cnt === 32'd50, "counts one per clock");
        // pressing the button asserts rst at once, without a clock edge
        #2 cpu_resetn = 1'b0;
        #1 check(dut.rst === 1'b1, "async assert");
        @(posedge clk); #1 check(dut.cnt === 32'd0, "count cleared");
        cpu_resetn = 1'b1;
        repeat (2) @(posedge clk);
        // LED mapping: preload the count and look at the top byte
        #1 dut.cnt = 32'hA5FF_FFFF;
        @(posedge clk); #1;
        check(led === 8'hA6, "led = cnt[31:24]");
        $display("counter32_top: reset, count and LED checks done at %0d ns", $time);
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
