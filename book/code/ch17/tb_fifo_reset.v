`timescale 1ns / 1ps
`default_nettype none

// Resetting an asynchronous FIFO: why both sides must be reset together.
// Write A1, B2, read both, then reset (a) the read side only, (b) both sides.
// (a) shows the failure: empty goes low with no new write and A1, B2 come out
// again. (b) is the correct procedure: the FIFO stays empty.
module tb_fifo_reset;
    reg wclk = 1'b0, rclk = 1'b0;
    always #5    wclk = ~wclk;            // 100 MHz
    always #6.85 rclk = ~rclk;            // about 73 MHz, unrelated

    reg        wrst = 1'b1, rrst = 1'b1, wr_en = 1'b0, rd_en = 1'b0;
    reg  [7:0] din = 8'h00;
    wire [7:0] dout;
    wire       full, empty;
    integer    k, errors = 0;

    async_fifo #(.W(8), .AW(4)) u_fifo (
        .wclk(wclk), .wrst(wrst), .wr_en(wr_en), .din(din), .full(full),
        .rclk(rclk), .rrst(rrst), .rd_en(rd_en), .dout(dout), .empty(empty));

    task write_two;
        begin
            @(negedge wclk) begin wr_en = 1'b1; din = 8'hA1; end
            @(negedge wclk) din = 8'hB2;
            @(negedge wclk) wr_en = 1'b0;
            repeat (6) @(posedge rclk);   // let the write pointer cross
        end
    endtask

    task read_two(input [8*8-1:0] tag);
        begin
            for (k = 0; k < 2; k = k + 1) begin
                @(negedge rclk) rd_en = 1'b1;
                @(negedge rclk) rd_en = 1'b0;
                #1 $display("  read %h%0s", dout, tag);
            end
        end
    endtask

    initial begin
        repeat (4) @(posedge rclk);
        @(negedge wclk) wrst = 1'b0;
        @(negedge rclk) rrst = 1'b0;

        $display("(a) write A1, B2, read them, then reset the READ side only");
        write_two;
        read_two("");
        repeat (6) @(posedge rclk);
        $display("  before reset: empty=%b", empty);
        @(negedge rclk) rrst = 1'b1;
        repeat (3) @(negedge rclk);
        rrst = 1'b0;
        repeat (6) @(posedge rclk);
        $display("  after read-side reset, no new write: empty=%b", empty);
        if (!empty) read_two(" (again)");

        $display("(b) stop both sides, reset BOTH sides, restart");
        @(negedge wclk) wrst = 1'b1;
        @(negedge rclk) rrst = 1'b1;
        repeat (3) @(negedge rclk);
        @(negedge wclk) wrst = 1'b0;
        @(negedge rclk) rrst = 1'b0;
        repeat (10) @(posedge rclk);
        $display("  after reset of both sides: empty=%b", empty);
        if (empty !== 1'b1) begin
            $display("FAIL: FIFO not empty after resetting both sides");
            errors = errors + 1;
        end
        write_two;
        read_two("");
        if (dout !== 8'hB2) begin
            $display("FAIL: last word %h, expected b2", dout);
            errors = errors + 1;
        end
        repeat (6) @(posedge rclk);
        if (empty !== 1'b1) begin
            $display("FAIL: FIFO not empty after reading both words");
            errors = errors + 1;
        end

        if (errors == 0) $display("PASS (both-side reset leaves the FIFO empty)");
        else             $display("FAILED with %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
