`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for udp_sender: the 19 bytes must match the
// UDP part of the frame built by build_udp_frame.py.
module tb_udp_sender;
    reg        clk = 1'b0, rst = 1'b1, start = 1'b0;
    wire [7:0] data;
    wire       valid, last;
    udp_sender dut (.clk(clk), .rst(rst), .start(start),
                    .data(data), .valid(valid), .last(last));
    always #4 clk = ~clk;                          // 125 MHz

    reg [7:0] exp_bytes [0:18];
    integer n = 0, errors = 0, k;

    initial begin
        exp_bytes[0]  = 8'h13; exp_bytes[1]  = 8'h88;   // source port 5000
        exp_bytes[2]  = 8'h13; exp_bytes[3]  = 8'h89;   // destination port 5001
        exp_bytes[4]  = 8'h00; exp_bytes[5]  = 8'h13;   // length 19
        exp_bytes[6]  = 8'h00; exp_bytes[7]  = 8'h00;   // checksum 0
        exp_bytes[8]  = 8'h48; exp_bytes[9]  = 8'h65; exp_bytes[10] = 8'h6c;
        exp_bytes[11] = 8'h6c; exp_bytes[12] = 8'h6f; exp_bytes[13] = 8'h20;
        exp_bytes[14] = 8'h57; exp_bytes[15] = 8'h6f; exp_bytes[16] = 8'h72;
        exp_bytes[17] = 8'h6c; exp_bytes[18] = 8'h64;
    end

    always @(posedge clk) begin
        if (valid) begin
            if (n > 18 || data !== exp_bytes[n] || last !== (n == 18)) begin
                $display("FAIL: byte %0d = %h (last %b)", n, data, last);
                errors = errors + 1;
            end
            n = n + 1;
        end
    end

    initial begin
        repeat (3) @(posedge clk);
        rst <= 1'b0;
        @(posedge clk) start <= 1'b1;
        @(posedge clk) start <= 1'b0;
        for (k = 0; k < 40; k = k + 1) @(posedge clk);
        if (n != 19) begin
            $display("FAIL: %0d bytes sent, expected 19", n); errors = errors + 1;
        end
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
