`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for coarse_tdc.
// 1) fixed intervals, 2) a start without a stop (timeout),
// 3) NEV muon-like events with exponential decay times (mean 2197 ns).
// Each measured time is written to muon_tdc_ns.txt for the histogram.
module tb_coarse_tdc;
    localparam NEV = 5000;
    reg  clk = 1'b0, rst = 1'b1, start_in = 1'b0, stop_in = 1'b0;
    wire [15:0] tdc_count;
    wire        valid, timeout;

    coarse_tdc #(.W(16), .TMAX(2000)) dut (
        .clk(clk), .rst(rst), .start_in(start_in), .stop_in(stop_in),
        .tdc_count(tdc_count), .valid(valid), .timeout(timeout));

    always #5 clk = ~clk;                         // 100 MHz

    integer errors = 0, seed = 476, i, dt, gap, fd, got, k;
    real    sum_ns = 0.0;

    // one event: start pulse, stop pulse dt ns later; returns measured ns
    task event_run(input integer dt_ns, output integer meas_ns);
        begin
            start_in = 1'b1; #20 start_in = 1'b0;
            #(dt_ns - 20) stop_in = 1'b1; #20 stop_in = 1'b0;
            meas_ns = -1;
            for (k = 0; k < 20 && meas_ns < 0; k = k + 1) begin
                @(posedge clk); #1;
                if (valid) meas_ns = 10 * tdc_count;
            end
        end
    endtask

    task check_one(input integer dt_ns, input integer meas_ns);
        if (meas_ns < 0 || meas_ns <= dt_ns - 10 || meas_ns >= dt_ns + 10) begin
            $display("FAIL: dt = %0d ns, measured %0d ns", dt_ns, meas_ns);
            errors = errors + 1;
        end
    endtask

    initial begin
        repeat (5) @(posedge clk);
        #1 rst = 1'b0;
        #103;

        // 1) fixed intervals, including non-integer phase to the clock
        event_run(1000, got); check_one(1000, got); #200;
        event_run(2203, got); check_one(2203, got); #200;
        event_run(37, got);   check_one(37, got);   #200;

        // 2) start without stop: expect timeout after TMAX counts
        start_in = 1'b1; #20 start_in = 1'b0;
        got = 0;
        for (k = 0; k < 2100 && !got; k = k + 1) begin
            @(posedge clk); #1;
            if (timeout) got = 1;
        end
        if (!got) begin
            $display("FAIL: no timeout"); errors = errors + 1;
        end
        #200;

        // 3) muon-like events
        fd = $fopen("muon_tdc_ns.txt", "w");
        for (i = 0; i < NEV; i = i + 1) begin
            dt = $dist_exponential(seed, 2197);
            while (dt < 30 || dt > 19000) dt = $dist_exponential(seed, 2197);
            gap = 100 + {$random(seed)} % 97;        // random phase to the clock
            #(gap);
            event_run(dt, got);
            check_one(dt, got);
            $fdisplay(fd, "%0d %0d", dt, got);
            sum_ns = sum_ns + got;
        end
        $fclose(fd);
        $display("mean measured time = %0.1f ns (%0d events)", sum_ns / NEV, NEV);
        if (sum_ns / NEV < 2100.0 || sum_ns / NEV > 2350.0) begin
            $display("FAIL: mean time out of range"); errors = errors + 1;
        end

        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
