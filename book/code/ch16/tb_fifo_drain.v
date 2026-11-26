`timescale 1ns / 1ps
`default_nettype none

// sync_fifo + fifo_drain: 20 words written in a burst must come out
// in order, none lost or doubled, also with a receiver that is
// sometimes not ready.
module tb_fifo_drain;
    reg clk = 0;
    always #5 clk = ~clk;

    reg        rst = 1, wr_en = 0, out_ready = 1;
    reg  [7:0] din = 0;
    wire [7:0] dout, out_data;
    wire       full, empty, rd_en, out_valid;

    sync_fifo #(.WIDTH(8), .AW(5)) u_fifo (.clk(clk), .rst(rst), .wr_en(wr_en),
        .din(din), .rd_en(rd_en), .dout(dout), .full(full), .empty(empty));
    fifo_drain #(.WIDTH(8)) u_drain (.clk(clk), .rst(rst), .empty(empty), .dout(dout),
        .rd_en(rd_en), .out_ready(out_ready), .out_valid(out_valid), .out_data(out_data));

    integer n_out = 0, errors = 0, i;
    always @(posedge clk)
        if (out_valid) begin
            if (out_data !== n_out + 1) begin
                $display("FAIL: word %0d = %0d", n_out, out_data);
                errors = errors + 1;
            end
            n_out = n_out + 1;
        end

    initial begin
        repeat (2) @(posedge clk);
        #1 rst = 0;
        for (i = 1; i <= 20; i = i + 1) begin   // burst: one word per clock
            wr_en = 1; din = i;
            @(posedge clk); #1;
            out_ready = ($urandom % 4) != 0;     // receiver busy 1/4 of the time
        end
        wr_en = 0;
        repeat (80) begin
            @(posedge clk); #1;
            out_ready = ($urandom % 4) != 0;
        end
        if (n_out != 20) begin
            $display("FAIL: %0d words out", n_out);
            errors = errors + 1;
        end
        if (errors == 0) $display("PASS (20 words drained in order)");
        $finish;
    end
endmodule

`default_nettype wire
