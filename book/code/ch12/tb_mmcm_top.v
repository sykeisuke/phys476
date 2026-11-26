`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for mmcm_top (optional Labs 9-11 of Chapter 12).
// Icarus: tb_mmcm_top.v mmcm_top.v reset_sync.v sim/clk_wiz_0_model.v
// It counts edges of the MMCM output clock clk_sys, so the checks do not
// depend on the output frequency.
module tb_mmcm_top;
    integer errors = 0;

    task check(input cond, input [8*24-1:0] what);
        if (!cond) begin
            $error("%0s", what);
            errors = errors + 1;
        end
    endtask

    reg clk = 1'b0;
    always #5 clk = ~clk;               // 100 MHz board clock

    reg cpu_resetn = 1'b0;
    wire [7:0] led;
    mmcm_top dut (.clk(clk), .cpu_resetn(cpu_resetn), .led(led));

    initial begin
        repeat (3) @(posedge clk);
        #1 cpu_resetn = 1'b1;           // MMCM leaves reset and starts to lock
        check(dut.locked !== 1'b1, "not locked yet");
        // the logic stays in reset until locked, then 2 more clk_sys edges
        wait (dut.locked === 1'b1);
        $display("locked at %0d ns", $time);
        check(dut.rst === 1'b1, "in reset at lock");
        repeat (2) @(posedge dut.clk_sys);
        #1 check(dut.rst === 1'b0, "reset released");
        check(dut.cnt === 32'd0, "starts from 0");
        repeat (50) @(posedge dut.clk_sys);
        #1 check(dut.cnt === 32'd50, "counts after release");
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
