`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for tdc_timestamp: hits at known times, some only
// 30 ns apart (a coarse start/stop TDC would miss these), checks that every
// hit gives one word and that time-stamp differences match within one count.
module tb_tdc_timestamp;
    localparam N = 200;
    reg  clk = 1'b0, rst = 1'b1, hit = 1'b0;
    wire [35:0] ts_data;
    wire        ts_valid;

    tdc_timestamp #(.W(32), .CH(5)) dut (.clk(clk), .rst(rst), .hit_in(hit),
                                         .ts_data(ts_data), .ts_valid(ts_valid));
    always #5 clk = ~clk;                      // 100 MHz

    integer seed = 19, i, gap, errors = 0, nout = 0;
    integer t_hit [0:N-1];
    reg [31:0] ts [0:N-1];

    always @(posedge clk) if (ts_valid) begin
        if (ts_data[35:32] !== 4'd5) begin
            $display("FAIL: channel %0d", ts_data[35:32]); errors = errors + 1;
        end
        if (nout < N) ts[nout] = ts_data[31:0];
        nout = nout + 1;
    end

    initial begin
        repeat (4) @(posedge clk);
        #1 rst = 1'b0;
        for (i = 0; i < N; i = i + 1) begin
            gap = (i % 4 == 1) ? 30 : 40 + {$random(seed)} % 500;   // ns, random phase
            #(gap);
            t_hit[i] = $realtime;
            hit = 1'b1; #15 hit = 1'b0;
        end
        #100;
        if (nout != N) begin
            $display("FAIL: %0d words for %0d hits", nout, N); errors = errors + 1;
        end
        for (i = 1; i < N && i < nout; i = i + 1)
            if ((ts[i] - ts[i-1]) * 10 <= t_hit[i] - t_hit[i-1] - 10 ||
                (ts[i] - ts[i-1]) * 10 >= t_hit[i] - t_hit[i-1] + 10) begin
                $display("FAIL: hit %0d: %0d counts for %0d ns", i, ts[i] - ts[i-1],
                         t_hit[i] - t_hit[i-1]);
                errors = errors + 1;
            end
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
