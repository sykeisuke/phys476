`timescale 1ns / 1ps
`default_nettype none

// Testbench for press_counter: a clean press counts once; a bouncing
// press counts several times. That is the bug the ILA shows on the board.
// Sources: tb_press_counter.v press_counter.v reset_sync.v
// It runs for about 5.2 us: in Vivado type  run all  (default stops at 1000 ns).
module tb_press_counter;
    integer errors = 0;
    integer i;

    task check(input cond, input [8*24-1:0] what);
        if (!cond) begin
            $error("%0s", what);
            errors = errors + 1;
        end
    endtask

    reg clk = 1'b0;
    always #5 clk = ~clk;

    reg cpu_resetn = 1'b0, btnc = 1'b0;
    wire [7:0] led;
    press_counter dut (.clk(clk), .cpu_resetn(cpu_resetn), .btnc(btnc), .led(led));

    initial begin
        repeat (3) @(posedge clk);
        cpu_resetn = 1'b1;
        check(led === 8'd0, "reset");

        // clean press: 1 for 1 us
        btnc = 1'b1; #1000; btnc = 1'b0; #1000;
        check(led === 8'd1, "clean press counts once");

        // bouncing press: contact opens and closes 3 times (200 ns each)
        for (i = 0; i < 3; i = i + 1) begin
            btnc = 1'b1; #200; btnc = 1'b0; #200;
        end
        btnc = 1'b1; #1000; btnc = 1'b0; #1000;
        check(led === 8'd5, "bounce counts 4 times");
        $display("after one clean and one bouncing press: count = %0d", led);

        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
