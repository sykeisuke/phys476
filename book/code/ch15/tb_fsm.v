`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for fsm_moore, fsm_mealy, fsm_led and edge_fsm.
module tb_fsm;
    reg clk = 1'b0;
    always #5 clk = ~clk;
    reg rst = 1'b1;
    reg go = 1'b0;
    reg ein = 1'b0;
    integer errors = 0;
    integer i, t_moore, t_mealy, n_moore, n_mealy, n_rise;

    task check(input cond, input [8*40-1:0] what);
        if (!cond) begin
            $display("FAIL at %0t: %0s", $time, what);
            errors = errors + 1;
        end
    endtask

    wire [3:0] c_moore, c_mealy;
    wire       d_moore, d_mealy, rise;
    wire [2:0] led;
    fsm_moore u_moore (.clk(clk), .rst(rst), .go(go), .count(c_moore), .done(d_moore));
    fsm_mealy u_mealy (.clk(clk), .rst(rst), .go(go), .count(c_mealy), .done(d_mealy));
    fsm_led #(.DIV(3), .W(2)) u_led (.clk(clk), .rst(rst), .led(led));
    edge_fsm u_edge (.clk(clk), .rst(rst), .in(ein), .rise(rise));

    // cycle counter and first cycle at which each 'done' is seen
    integer cyc = 0;
    always @(negedge clk) begin
        cyc = cyc + 1;
        if (d_moore) begin n_moore = n_moore + 1; if (t_moore < 0) t_moore = cyc; end
        if (d_mealy) begin n_mealy = n_mealy + 1; if (t_mealy < 0) t_mealy = cyc; end
        if (rise)    n_rise = n_rise + 1;
    end

    reg [2:0] expect_led [0:3];
    initial begin
        expect_led[0] = 3'b001; expect_led[1] = 3'b010;
        expect_led[2] = 3'b100; expect_led[3] = 3'b000;
        t_moore = -1; t_mealy = -1; n_moore = 0; n_mealy = 0; n_rise = 0;

        repeat (2) @(posedge clk);
        #1 rst = 1'b0;

        // --- fsm_led: one step every 3 clocks, pattern 001 010 100 000 001
        check(led == 3'b001, "led starts in S0");
        for (i = 1; i <= 8; i = i + 1) begin
            repeat (3) @(posedge clk);
            #1 check(led == expect_led[i % 4], "led pattern step");
        end

        // --- Moore vs Mealy: start both with go
        @(posedge clk) #1 go = 1'b1;
        repeat (25) @(posedge clk);
        #1;
        check(t_mealy > 0 && t_moore == t_mealy + 1, "Moore done one cycle after Mealy");
        check(n_mealy == 1, "Mealy done is one cycle");
        check(d_moore == 1'b1, "Moore done held while go = 1");
        go = 1'b0;
        repeat (3) @(posedge clk);
        #1 check(d_moore == 1'b0 && c_moore == 4'd0, "Moore back in IDLE");

        // --- edge_fsm: three long presses give three pulses
        for (i = 0; i < 3; i = i + 1) begin
            @(posedge clk) #1 ein = 1'b1;
            repeat (6) @(posedge clk);
            #1 ein = 1'b0;
            repeat (4) @(posedge clk);
        end
        #1 check(n_rise == 3, "edge_fsm: one pulse per press");

        if (errors == 0)
            $display("PASS (moore/mealy done %0d/%0d, led 8 steps, 3 edges)", t_moore, t_mealy);
        else
            $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
