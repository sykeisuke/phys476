`timescale 1ns / 1ps
`default_nettype none

// Testbench for the Chapter 28 helpers.
// Part 1: read a text file with $fopen/$fscanf (Project 1 level 1 reads its events this way).
// Part 2: the 16-bit LFSR has period 65535 (Project 2 level 2 pulse generator).
// Run:  python3 make_events.py
//       iverilog -g2005 -Wall -o tb_ch28 tb_ch28.v lfsr16.v && vvp tb_ch28
module tb_ch28;
    // ---------------- part 1: $fscanf
    integer fd, n_read, n_ev, t_start, t_stop, sum_dt, n_bad, prev_stop, n_ovl;
    // ---------------- part 2: LFSR
    reg clk = 1'b0, rst = 1'b1, en = 1'b0;
    wire [15:0] q;
    integer period;
    lfsr16 dut (.clk(clk), .rst(rst), .en(en), .q(q));
    always #5 clk = ~clk;                       // 100 MHz

    initial begin
        // $fopen returns a file handle (0 = file not found);
        // $fscanf returns how many numbers it read (-1 at the end of the file).
        fd = $fopen("events.txt", "r");
        if (fd == 0) begin
            $display("FAIL: events.txt not found (run python3 make_events.py first)");
            $finish;
        end
        n_ev = 0; sum_dt = 0; n_bad = 0; prev_stop = 0; n_ovl = 0;
        n_read = $fscanf(fd, "%d %d\n", t_start, t_stop);
        while (n_read == 2) begin
            n_ev = n_ev + 1;
            sum_dt = sum_dt + (t_stop - t_start);
            if (t_stop <= t_start) n_bad = n_bad + 1;
            if (t_start <= prev_stop) n_ovl = n_ovl + 1;   // starts before the last stop
            prev_stop = t_stop;
            n_read = $fscanf(fd, "%d %d\n", t_start, t_stop);
        end
        $fclose(fd);
        $display("read %0d events, sum of decay times %0d ns, %0d overlapping",
                 n_ev, sum_dt, n_ovl);

        // LFSR: count clocks until the reset state comes back
        repeat (2) @(posedge clk);
        rst <= 1'b0; en <= 1'b1;
        @(posedge clk);                         // first step leaves 16'h0001
        period = 0;
        @(posedge clk);
        period = 1;
        while (q != 16'h0001 && period < 70000) begin
            @(posedge clk);
            period = period + 1;
        end
        $display("LFSR period %0d", period);

        if (n_ev == 1000 && n_bad == 0 && n_ovl == 0 && period == 65535) $display("PASS");
        else $display("FAIL");
        $finish;
    end
endmodule
