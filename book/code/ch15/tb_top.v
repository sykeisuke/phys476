`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for the two board tops (short parameters).
module tb_top;
    integer errors = 0;
    reg clk = 1'b0;
    always #5 clk = ~clk;

    task check(input cond, input [8*40-1:0] what);
        if (!cond) begin
            $display("FAIL at %0t: %0s", $time, what);
            errors = errors + 1;
        end
    endtask

    reg        cpu_resetn = 1'b0, btnc = 1'b0, btnu = 1'b0;
    wire [7:0] led_a, led_b;

    // LED pattern: one step every 4 clocks
    fsm_led_top #(.DIV(4), .W(3)) u_a (.clk(clk), .cpu_resetn(cpu_resetn), .led(led_a));

    // trigger/busy: tick every 10 clocks, K = 2, readout 300 clocks
    trig_busy_top #(.DIV(10), .TW(4), .K(2), .RD_CYCLES(300), .RW(9))
        u_b (.clk(clk), .cpu_resetn(cpu_resetn), .btnc(btnc), .btnu(btnu), .led(led_b));

    // LED sequence seen by sampling every 4 clocks
    reg [2:0] seq [0:7];
    integer k;

    task press(input integer hold);
        begin
            btnc = 1'b1;
            repeat (hold) @(negedge clk);
            btnc = 1'b0;
            repeat (60) @(negedge clk);
        end
    endtask

    initial begin
        repeat (4) @(negedge clk);
        cpu_resetn = 1'b1;
        repeat (3) @(negedge clk);

        // ---- LED FSM: the pattern must run 001 010 100 000 001 ...
        @(posedge clk);
        while (led_a[2:0] != 3'b001) @(posedge clk);
        for (k = 0; k < 8; k = k + 1) begin
            seq[k] = led_a[2:0];
            repeat (4) @(posedge clk);
        end
        check(seq[0] == 3'b001 && seq[1] == 3'b010 && seq[2] == 3'b100 &&
              seq[3] == 3'b000 && seq[4] == 3'b001, "LED pattern 001 010 100 000");
        check(led_a[7:3] == 5'b0, "LED[7:3] off");

        // ---- trigger/busy top
        @(negedge clk);
        check(led_b == 8'h00, "idle after reset");
        press(80);                                   // accepted
        check(led_b[0] == 1'b1, "busy after press");
        check(led_b[7:4] == 4'd1, "1 accepted");
        press(80);                                   // during readout: rejected
        check(led_b[3:1] == 3'd1, "1 rejected");
        repeat (300) @(negedge clk);
        check(led_b[0] == 1'b0, "not busy after readout");
        btnu = 1'b1;                                 // buffer full
        repeat (60) @(negedge clk);
        check(led_b[0] == 1'b1, "busy while full");
        press(80);                                   // rejected (HOLD)
        btnu = 1'b0;
        repeat (60) @(negedge clk);
        check(led_b[0] == 1'b0, "not busy after full released");
        check(led_b == {4'd1, 3'd2, 1'b0}, "LEDs: 1 accepted, 2 rejected");

        if (errors == 0) $display("PASS (LED pattern, trigger/busy top: led = %b)", led_b);
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
