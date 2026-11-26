`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for Chapter 17, with two unrelated clocks.
module tb_ch17;
    integer i, k, ones, errors = 0;

    reg clk_a = 1'b0, clk_b = 1'b0;
    always #5.0  clk_a = ~clk_a;     // 100 MHz
    always #6.85 clk_b = ~clk_b;     // about 73 MHz, unrelated

    // ---------------------------------------------------- Gray code (8 bits)
    reg  [7:0] bin, nbin;
    wire [7:0] g, gn, back;
    bin2gray #(.N(8)) u_b2g  (.bin(bin),  .gray(g));
    bin2gray #(.N(8)) u_b2gn (.bin(nbin), .gray(gn));
    gray2bin #(.N(8)) u_g2b  (.gray(g),   .bin(back));

    // ---------------------------------------------------- pulse synchronizer
    reg  pulse_a = 1'b0;
    wire pulse_b;
    pulse_sync u_ps (.clk_a(clk_a), .pulse_a(pulse_a), .clk_b(clk_b), .pulse_b(pulse_b));
    integer n_sent = 0, n_recv = 0;
    always @(posedge clk_b)
        if (pulse_b) n_recv = n_recv + 1;

    // ---------------------------------------------------- handshake (bus)
    reg        send = 1'b0;
    reg  [7:0] data_a = 8'd0;
    wire       busy, valid_b;
    wire [7:0] data_b;
    handshake_sync #(.W(8)) u_hs (.clk_a(clk_a), .send(send), .data_a(data_a), .busy(busy),
                                  .clk_b(clk_b), .data_b(data_b), .valid_b(valid_b));
    integer n_words = 0;
    reg [7:0] expect_b = 8'd0;
    always @(posedge clk_b)
        if (valid_b) begin
            if (data_b !== expect_b) begin
                $display("FAIL: handshake got %0d, expected %0d", data_b, expect_b);
                errors = errors + 1;
            end
            expect_b = expect_b + 8'd37;
            n_words = n_words + 1;
        end

    // ---------------------------------------------------- reset synchronizer
    reg  arst_n = 1'b1;
    wire rst_b;
    reset_sync u_rs (.clk(clk_b), .arst_n(arst_n), .rst(rst_b));

    // ---------------------------------------------------- asynchronous FIFO
    // writer on clk_a, reader on clk_b; reference queue q[]
    reg        f_rst = 1'b1, wr_en = 1'b0, rd_en = 1'b0;
    reg  [7:0] din = 8'd0;
    wire [7:0] dout;
    wire       full, empty;
    async_fifo #(.W(8), .AW(4)) u_af (
        .wclk(clk_a), .wrst(f_rst), .wr_en(wr_en), .din(din), .full(full),
        .rclk(clk_b), .rrst(f_rst), .rd_en(rd_en), .dout(dout), .empty(empty));
    reg  [7:0] q [0:1023];
    integer n_wr = 0, n_rd = 0, n_full = 0, rd_pend = 0;
    reg  [7:0] exp_d = 8'd0;
    always @(posedge clk_a)
        if (!f_rst && wr_en && !full) begin
            q[n_wr % 1024] = din;
            n_wr = n_wr + 1;
        end else if (!f_rst && wr_en && full)
            n_full = n_full + 1;
    always @(posedge clk_b) begin
        if (rd_pend) begin
            if (dout !== exp_d) begin
                $display("FAIL: async FIFO read %0d, expected %0d", dout, exp_d);
                errors = errors + 1;
            end
        end
        rd_pend = 0;
        if (!f_rst && rd_en && !empty) begin
            if (n_rd >= n_wr) begin
                $display("FAIL: async FIFO read while model empty");
                errors = errors + 1;
            end
            exp_d   = q[n_rd % 1024];
            n_rd    = n_rd + 1;
            rd_pend = 1;
        end
    end

    initial begin
        // Gray code: round trip, and one-bit change between neighbours (incl. wrap)
        for (i = 0; i < 256; i = i + 1) begin
            bin  = i;
            nbin = i + 1;            // 255 + 1 wraps to 0
            #1;
            if (back !== bin) begin
                $display("FAIL: gray2bin(bin2gray(%0d)) = %0d", bin, back);
                errors = errors + 1;
            end
            ones = 0;
            for (k = 0; k < 8; k = k + 1)
                ones = ones + ((g[k] ^ gn[k]) ? 1 : 0);
            if (ones != 1) begin
                $display("FAIL: Gray codes of %0d and %0d differ in %0d bits", bin, nbin, ones);
                errors = errors + 1;
            end
        end

        // Pulses: 50 one-cycle pulses, 6 to 13 clk_a cycles apart (> 3 clk_b cycles)
        @(negedge clk_a);
        for (i = 0; i < 50; i = i + 1) begin
            pulse_a = 1'b1;
            n_sent  = n_sent + 1;
            @(negedge clk_a);
            pulse_a = 1'b0;
            repeat (5 + (i % 8)) @(negedge clk_a);
        end
        repeat (10) @(negedge clk_b);
        if (n_recv != n_sent) begin
            $display("FAIL: sent %0d pulses, received %0d", n_sent, n_recv);
            errors = errors + 1;
        end

        // Bus: 20 words through the handshake, sent as fast as busy allows
        @(negedge clk_a);
        for (i = 0; i < 20; i = i + 1) begin
            while (busy) @(negedge clk_a);
            data_a = i * 37;
            send   = 1'b1;
            @(negedge clk_a);
            send   = 1'b0;
            data_a = 8'hXX;          // source may change data after the request
        end
        repeat (20) @(negedge clk_b);
        if (n_words != 20) begin
            $display("FAIL: received %0d of 20 words", n_words);
            errors = errors + 1;
        end

        // Reset synchronizer: asserts at once, releases on the 2nd clk_b edge
        repeat (3) @(posedge clk_b);
        #3 arst_n = 1'b0;
        #1 if (rst_b !== 1'b1) begin $display("FAIL: reset not asserted at once"); errors = errors + 1; end
        #20 arst_n = 1'b1;
        @(posedge clk_b) #1 if (rst_b !== 1'b1) begin $display("FAIL: reset released too early"); errors = errors + 1; end
        @(posedge clk_b) #1 if (rst_b !== 1'b0) begin $display("FAIL: reset not released"); errors = errors + 1; end

        // Asynchronous FIFO: random writes and reads, writer faster than reader
        repeat (3) @(posedge clk_b);
        f_rst = 1'b0;
        for (i = 0; i < 3000; i = i + 1) begin
            @(negedge clk_a);
            wr_en = (n_wr < 400) && ($urandom % 4 != 0);
            din   = $urandom_range(255, 1);
            if (i % 7 == 0) @(negedge clk_b);
            rd_en = ($urandom % 3 != 0);
        end
        @(negedge clk_a) wr_en = 1'b0;
        rd_en = 1'b1;
        repeat (100) @(negedge clk_b);
        if (n_rd != n_wr || n_wr != 400 || !empty) begin
            $display("FAIL: async FIFO wrote %0d, read %0d", n_wr, n_rd);
            errors = errors + 1;
        end
        if (n_full == 0) begin
            $display("FAIL: async FIFO never became full");
            errors = errors + 1;
        end

        if (errors == 0)
            $display("PASS: gray 256 codes, %0d/%0d pulses, %0d/20 words, reset_sync, async FIFO %0d words (full %0d times)",
                     n_recv, n_sent, n_words, n_rd, n_full);
        else
            $display("FAILED with %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
