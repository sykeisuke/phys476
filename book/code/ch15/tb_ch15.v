`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for the Chapter 15 FSMs.
module tb_ch15;
    reg clk = 1'b0;
    always #5 clk = ~clk;                 // 100 MHz
    reg rst = 1'b1;
    integer errors = 0;
    integer i;

    task check(input cond, input [8*32-1:0] what);
        if (!cond) begin
            $display("FAIL at %0t: %0s", $time, what);
            errors = errors + 1;
        end
    endtask

    // ------------------------------------------------------------ framer
    reg         start = 1'b0;
    wire [15:0] din, dout;
    wire        next_word, dout_valid, fbusy;
    framer #(.N(4)) u_fr (.clk(clk), .rst(rst), .start(start), .din(din),
                          .next_word(next_word), .dout(dout), .dout_valid(dout_valid),
                          .busy(fbusy));

    // data source: a counter showing 1001, 1002, ...; it moves on
    // to the next word after each cycle with next_word = 1
    reg [3:0] widx = 4'd0;
    assign din = 16'h1001 + widx;
    always @(posedge clk) if (next_word) widx <= widx + 4'd1;

    // record the output stream
    reg [15:0] got [0:31];
    integer ngot = 0;
    always @(posedge clk)
        if (dout_valid) begin
            got[ngot] = dout;
            ngot = ngot + 1;
        end

    // ------------------------------------------------------------ trig_busy
    reg         trig = 1'b0, rd_done = 1'b0, full = 1'b0;
    wire        busy, start_rd;
    wire [15:0] n_acc, n_rej;
    trig_busy u_tb (.clk(clk), .rst(rst), .trig(trig), .rd_done(rd_done),
                    .full(full), .busy(busy), .start_rd(start_rd),
                    .n_acc(n_acc), .n_rej(n_rej));

    // readout model: rd_done 6 cycles after start_rd
    integer nstart = 0;
    always @(posedge clk) if (start_rd) begin
        nstart = nstart + 1;
        repeat (5) @(posedge clk);
        @(negedge clk) rd_done = 1'b1;
        @(negedge clk) rd_done = 1'b0;
    end

    task pulse_trig;
        begin
            @(negedge clk) trig = 1'b1;
            @(negedge clk) trig = 1'b0;
        end
    endtask

    initial begin
        repeat (3) @(negedge clk);
        rst = 1'b0;

        // ---- framer: two frames; a start during the first is ignored
        @(negedge clk) start = 1'b1;
        @(negedge clk) start = 1'b0;
        check(fbusy == 1'b1, "framer busy after start");
        repeat (2) @(negedge clk);
        start = 1'b1;                     // while busy: must be ignored
        @(negedge clk) start = 1'b0;
        repeat (10) @(negedge clk);
        check(fbusy == 1'b0, "framer idle after frame");
        check(ngot == 6, "frame 1 has 6 words");
        @(negedge clk) start = 1'b1;
        @(negedge clk) start = 1'b0;
        repeat (10) @(negedge clk);
        check(ngot == 12, "two frames, 12 words");
        for (i = 0; i < 2; i = i + 1) begin
            check(got[6*i] == 16'hA5A5, "header");
            check(got[6*i+1] == 16'h1001 + 4*i, "data word 0");
            check(got[6*i+2] == 16'h1002 + 4*i, "data word 1");
            check(got[6*i+3] == 16'h1003 + 4*i, "data word 2");
            check(got[6*i+4] == 16'h1004 + 4*i, "data word 3");
            check(got[6*i+5] == 16'h5A5A, "footer");
        end

        // ---- trig_busy
        check(busy == 1'b0, "idle after reset");
        pulse_trig;                       // accepted
        check(busy == 1'b1, "busy one cycle after trigger");
        repeat (2) @(negedge clk);
        pulse_trig;                       // rejected (busy)
        repeat (10) @(negedge clk);
        check(busy == 1'b0, "idle after readout");
        pulse_trig;                       // accepted
        repeat (12) @(negedge clk);
        full = 1'b1;                      // buffer full
        @(negedge clk);
        check(busy == 1'b1, "busy while full");
        pulse_trig;                       // rejected (HOLD)
        repeat (3) @(negedge clk);
        full = 1'b0;
        repeat (2) @(negedge clk);
        check(busy == 1'b0, "idle after full clears");
        pulse_trig;                       // accepted
        repeat (12) @(negedge clk);
        check(n_acc == 16'd3, "3 accepted triggers");
        check(n_rej == 16'd2, "2 rejected triggers");
        check(nstart == 3, "3 readouts started");

        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
