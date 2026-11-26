`timescale 1ns / 1ps
`default_nettype none

// Exhaustive check of the hierarchy example: out must equal ~in.
module tb_hier;
    reg  [2:0] in;
    wire [2:0] out;
    integer i, errors = 0;

    top dut (.in(in), .out(out));

    initial begin
        for (i = 0; i < 8; i = i + 1) begin
            in = i[2:0];
            #1;
            if (out !== ~in) begin
                $error("in=%b: out=%b, want %b", in, out, ~in);
                errors = errors + 1;
            end
        end
        // hierarchical name: look inside an instance from the testbench
        $display("u1.w1 = %b (leaf_A of bit 1)", dut.u1.w1);
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
