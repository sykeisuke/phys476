`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for the Chapter 14 blocks.
module tb_ch14;
    integer errors = 0;
    integer i, n;

    reg clk = 1'b0;
    always #5 clk = ~clk;          // 100 MHz

    task check(input cond, input [8*40-1:0] what);
        if (!cond) begin
            $display("FAIL: %0s", what);
            errors = errors + 1;
        end
    endtask

    // ---------------------------------------------------------- tick_gen
    reg  rst = 1'b1;
    wire tick;
    tick_gen #(.DIV(5), .W(3)) u_tick (.clk(clk), .rst(rst), .tick(tick));

    // ---------------------------------------------------------- oneshot
    reg  trig = 1'b0, retrig = 1'b0;
    wire os_out;
    oneshot #(.N(4), .W(3)) u_os (.clk(clk), .rst(rst), .trig(trig),
                                  .retrig(retrig), .out(os_out));

    // ---------------------------------------------------------- sync_2ff
    reg  a_in = 1'b0;
    wire a_s;
    sync_2ff u_sync (.clk(clk), .d_async(a_in), .q(a_s));

    // ---------------------------------------------------------- edge_detect
    reg  e_in = 1'b0;
    wire e_rise;
    edge_detect u_edge (.clk(clk), .in(e_in), .rise(e_rise));

    // ---------------------------------------------------------- sw_counter
    // fast settings: tick every 10 clocks, 4 equal samples (= 40 clocks)
    reg  btnc = 1'b0;
    reg  cpu_resetn = 1'b0;
    wire [7:0] led;
    sw_counter #(.DIV(10), .W(4), .K(4)) u_sw (.clk(clk), .cpu_resetn(cpu_resetn),
                                               .btnc(btnc), .led(led));

    // count cycles where a signal is 1 (sampled mid-cycle, no race)
    integer n_tick = 0, n_os = 0, n_rise = 0;
    always @(negedge clk) begin
        if (tick)   n_tick = n_tick + 1;
        if (os_out) n_os   = n_os + 1;
        if (e_rise) n_rise = n_rise + 1;
    end

    // a bouncing transition of btnc towards 'level' (at most 300 ns, i.e.
    // shorter than K = 4 ticks of 100 ns), then hold the level
    task bounce_to(input level, input integer hold_ns);
        integer k;
        begin
            for (k = 0; k < 10; k = k + 1) begin
                btnc = ~level;
                #($urandom_range(15, 1));
                btnc = level;
                #($urandom_range(15, 1));
            end
            btnc = level;
            #(hold_ns);
        end
    endtask

    initial begin
        repeat (3) @(posedge clk);
        #1 rst = 1'b0;

        // tick_gen: 10 ticks in 50 cycles, each one cycle wide
        @(posedge clk) #1 n_tick = 0;
        repeat (50) @(posedge clk);
        #1 check(n_tick == 10, "tick_gen: 10 ticks in 50 cycles");

        // oneshot: one trigger -> 4 cycles high
        n_os = 0;
        @(posedge clk) #1 trig = 1'b1;
        @(posedge clk) #1 trig = 1'b0;
        repeat (10) @(posedge clk);
        check(n_os == 4, "oneshot: pulse is 4 cycles");

        // oneshot, retrig = 0: a second trigger during the pulse is ignored
        n_os = 0;
        @(posedge clk) #1 trig = 1'b1;
        @(posedge clk) #1 trig = 1'b0;
        @(posedge clk) #1 trig = 1'b1;
        @(posedge clk) #1 trig = 1'b0;
        repeat (10) @(posedge clk);
        check(n_os == 4, "oneshot: retrig=0 ignores 2nd trig");

        // oneshot, retrig = 1: the second trigger restarts the pulse
        n_os = 0;
        retrig = 1'b1;
        @(posedge clk) #1 trig = 1'b1;
        @(posedge clk) #1 trig = 1'b0;
        @(posedge clk) #1 trig = 1'b1;
        @(posedge clk) #1 trig = 1'b0;
        repeat (10) @(posedge clk);
        check(n_os == 6, "oneshot: retrig=1 extends pulse");
        retrig = 1'b0;

        // sync_2ff: output follows after two clock edges
        #3 a_in = 1'b1;                 // asynchronous change
        @(posedge clk) #1 check(a_s == 1'b0, "sync_2ff: not after 1 edge");
        @(posedge clk) #1 check(a_s == 1'b1, "sync_2ff: after 2 edges");
        #2 a_in = 1'b0;
        repeat (2) @(posedge clk);
        #1 check(a_s == 1'b0, "sync_2ff: follows 1->0");

        // edge_detect: a 5-cycle high level gives exactly one pulse
        n_rise = 0;
        @(posedge clk) #1 e_in = 1'b1;
        repeat (5) @(posedge clk);
        #1 e_in = 1'b0;
        repeat (5) @(posedge clk);
        check(n_rise == 1, "edge_detect: one pulse per edge");

        // sw_counter: release reset
        repeat (3) @(posedge clk);
        #2 cpu_resetn = 1'b1;
        repeat (5) @(posedge clk);
        if (led != 8'd0) begin
            $display("FAIL: sw_counter after reset: led = %0d (expected 0)", led);
            errors = errors + 1;
        end

        // short glitches (shorter than K ticks) must not count
        for (i = 0; i < 5; i = i + 1) begin
            #7 btnc = 1'b1;
            #($urandom_range(25, 3)) btnc = 1'b0;
            #50;
        end
        #300;
        if (led != 8'd0) begin
            $display("FAIL: sw_counter, 5 glitches: led = %0d (expected 0)", led);
            errors = errors + 1;
        end

        // 7 bouncing presses -> exactly 7 counts
        n = 7;
        for (i = 0; i < n; i = i + 1) begin
            bounce_to(1'b1, 800);       // press and hold 80 clocks
            bounce_to(1'b0, 800);       // release
        end
        if (led != 8'd7) begin
            $display("FAIL: sw_counter, 7 presses: led = %0d (expected 7)", led);
            errors = errors + 1;
        end

        if (errors == 0)
            $display("PASS (tick, oneshot, sync_2ff, edge_detect, sw_counter = %0d)", led);
        else
            $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
