// hello_tb.v -- self-checking testbench for hello.v.
// Run:  iverilog -g2005 -Wall -o hello_tb hello_tb.v hello.v && vvp hello_tb
// Expected last line:  PASS
`timescale 1ns / 1ps
`default_nettype none

module hello_tb;
    reg  a, b;
    wire y;
    integer i;
    integer errors;

    hello dut (.a(a), .b(b), .y(y));

    initial begin
        errors = 0;
        for (i = 0; i < 4; i = i + 1) begin
            {a, b} = i[1:0];          // 00, 01, 10, 11
            #10;
            $display("a=%b b=%b  y=%b", a, b, y);
            if (y !== (a & b)) begin
                $display("FAIL: a=%b b=%b y=%b", a, b, y);
                errors = errors + 1;
            end
        end
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
