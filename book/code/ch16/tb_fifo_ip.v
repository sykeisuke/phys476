`timescale 1ns / 1ps
`default_nettype none

// Lab 2 testbench for the Vivado FIFO Generator (fifo_generator_0:
// Native, Common Clock Block RAM, Standard FIFO, 8 bits wide, smallest
// depth, synchronous reset srst). Add it as a simulation source next to
// the IP and run Behavioral Simulation, then type "run all" in the Tcl
// console (the default 1000 ns is too short).
// The test does not assume the depth: it writes until full, prints how
// many words fitted, then reads everything back and checks the order.
// Without Vivado, run it in Icarus with the stand-in model:
//   iverilog -g2005 -Wall -o tb_ip tb_fifo_ip.v fifo_ip_model.v sync_fifo.v
module tb_fifo_ip;
    reg clk = 0;
    always #5 clk = ~clk;            // 100 MHz

    reg        srst = 1, wr_en = 0, rd_en = 0;
    reg  [7:0] din = 0;
    wire [7:0] dout;
    wire       full, empty;

    fifo_generator_0 dut (
        .clk(clk), .srst(srst), .din(din), .wr_en(wr_en), .rd_en(rd_en),
        .dout(dout), .full(full), .empty(empty));

    integer n_wr = 0, n_rd = 0, errors = 0;

    initial begin
        // hold reset, then wait: the IP needs some clocks after reset
        // before it accepts writes (see PG057, "reset behaviour")
        repeat (5) @(posedge clk);
        srst <= 1'b0;
        repeat (20) @(posedge clk);
        if (!empty) begin $display("FAIL: not empty after reset"); errors = errors + 1; end

        // write 8'h10, 8'h11, ... until full (at most 64 tries)
        while (!full && n_wr < 64) begin
            wr_en <= 1'b1; din <= 8'h10 + n_wr;
            @(posedge clk);
            n_wr = n_wr + 1;
            #1;                          // let the flags settle
        end
        wr_en <= 1'b0;
        @(posedge clk); #1;
        $display("full after %0d writes", n_wr);
        if (!full) begin $display("FAIL: never full"); errors = errors + 1; end

        // read until empty; Standard FIFO: dout is valid one clock after rd_en
        while (!empty && n_rd < 64) begin
            rd_en <= 1'b1;
            @(posedge clk); #1;
            rd_en <= 1'b0;
            if (dout !== 8'h10 + n_rd) begin
                $display("FAIL: read %0d gave %h, expected %h", n_rd, dout, 8'h10 + n_rd);
                errors = errors + 1;
            end
            n_rd = n_rd + 1;
        end
        if (n_rd != n_wr) begin
            $display("FAIL: wrote %0d, read %0d", n_wr, n_rd); errors = errors + 1;
        end
        if (errors == 0) $display("PASS (depth %0d, %0d words read back in order)", n_wr, n_rd);
        $finish;
    end
endmodule

`default_nettype wire
