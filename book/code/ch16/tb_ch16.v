`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for sync_fifo and bram_sdp.
// A reference queue (array + head/tail counters) predicts every output.
module tb_ch16;
    localparam W = 8, AW = 3, DEPTH = 1 << AW;

    reg clk = 0;
    always #5 clk = ~clk;            // 100 MHz

    reg          rst = 1, wr_en = 0, rd_en = 0;
    reg  [W-1:0] din = 0;
    wire [W-1:0] dout;
    wire         full, empty;
    sync_fifo #(.WIDTH(W), .AW(AW)) dut (
        .clk(clk), .rst(rst), .wr_en(wr_en), .din(din),
        .rd_en(rd_en), .dout(dout), .full(full), .empty(empty));

    // reference model
    reg [W-1:0] q [0:1023];
    integer head = 0, tail = 0, errors = 0, i, n_rd = 0;
    reg [W-1:0] expect_d;
    reg         check_next = 0;

    // one FIFO cycle: drive inputs, clock, update the model
    task cycle(input w, input r);
        reg dw, dr;
        begin
            wr_en = w; rd_en = r;
            din = $urandom_range(255, 1);   // 8-bit data, never 0
            dw = w && (tail - head < DEPTH);
            dr = r && (tail - head > 0);
            if (full  !== (tail - head == DEPTH)) begin
                $display("FAIL: full=%b, model count=%0d", full, tail - head);
                errors = errors + 1;
            end
            if (empty !== (tail - head == 0)) begin
                $display("FAIL: empty=%b, model count=%0d", empty, tail - head);
                errors = errors + 1;
            end
            @(posedge clk);
            if (dw) begin q[tail % 1024] = din; tail = tail + 1; end
            if (dr) begin expect_d = q[head % 1024]; head = head + 1; end
            #1;
            if (dr) begin
                n_rd = n_rd + 1;
                if (dout !== expect_d) begin
                    $display("FAIL: dout=%0d, expected %0d", dout, expect_d);
                    errors = errors + 1;
                end
            end
        end
    endtask

    // bram_sdp
    reg         we = 0;
    reg  [9:0]  wa = 0, ra = 0;
    reg  [15:0] bd = 0;
    wire [15:0] bq;
    bram_sdp u_ram (.clk(clk), .we(we), .waddr(wa), .din(bd), .raddr(ra), .dout(bq));

    initial begin
        $dumpfile("tb_ch16.vcd");       // open with GTKWave or Surfer
        $dumpvars(0, tb_ch16);
        @(posedge clk); @(posedge clk);
        #1 rst = 0;
        // 1) fill past full: the extra writes must be ignored
        for (i = 0; i < DEPTH + 3; i = i + 1) cycle(1, 0);
        if (!full) begin $display("FAIL: not full"); errors = errors + 1; end
        // 2) drain past empty
        for (i = 0; i < DEPTH + 3; i = i + 1) cycle(0, 1);
        if (!empty) begin $display("FAIL: not empty"); errors = errors + 1; end
        // 3) simultaneous read and write at half fill
        for (i = 0; i < DEPTH / 2; i = i + 1) cycle(1, 0);
        for (i = 0; i < 20; i = i + 1) cycle(1, 1);
        // 4) random traffic
        for (i = 0; i < 2000; i = i + 1) cycle($urandom % 2, $urandom % 2);

        // bram_sdp: write a pattern, read back with 1-cycle latency
        wr_en = 0; rd_en = 0;
        for (i = 0; i < 16; i = i + 1) begin
            we = 1; wa = i; bd = 16'hA500 + i;
            @(posedge clk); #1;
        end
        we = 0;
        for (i = 0; i < 16; i = i + 1) begin
            ra = i;
            @(posedge clk); #1;
            if (bq !== 16'hA500 + i) begin
                $display("FAIL: ram[%0d] = %h", i, bq);
                errors = errors + 1;
            end
        end

        if (errors == 0) $display("PASS (%0d FIFO reads checked)", n_rd);
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
