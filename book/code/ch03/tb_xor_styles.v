`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench: the five XOR descriptions agree with a ^ b.
module tb_xor_styles;
    reg  a, b;
    wire y_op, y_nand, y_table, y_case, y_inst;
    integer i, errors = 0;

    xor_op    u1 (.a(a), .b(b), .y(y_op));
    xor_nand  u2 (.a(a), .b(b), .y(y_nand));
    xor_table u3 (.a(a), .b(b), .y(y_table));
    xor_case  u4 (.a(a), .b(b), .y(y_case));
    xor_inst  u5 (.a(a), .b(b), .y(y_inst));

    initial begin
        for (i = 0; i < 4; i = i + 1) begin
            {a, b} = i[1:0];
            #1;
            if ({y_op, y_nand, y_table, y_case, y_inst} !== {5{a ^ b}}) begin
                $error("a=%b b=%b: op=%b nand=%b table=%b case=%b inst=%b",
                       a, b, y_op, y_nand, y_table, y_case, y_inst);
                errors = errors + 1;
            end
        end
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
