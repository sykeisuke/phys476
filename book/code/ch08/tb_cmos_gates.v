`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench: the switch-level gates against Verilog operators.
module tb_cmos_gates;
    reg  a, b;
    wire y_inv, y_nand, y_nor;
    integer i, errors = 0;

    cmos_inv   u_inv  (.a(a), .y(y_inv));
    cmos_nand2 u_nand (.a(a), .b(b), .y(y_nand));
    cmos_nor2  u_nor  (.a(a), .b(b), .y(y_nor));

    initial begin
        $display(" a b | NOT a  NAND  NOR");
        for (i = 0; i < 4; i = i + 1) begin
            {a, b} = i[1:0];
            #1;
            $display(" %b %b |   %b      %b     %b", a, b, y_inv, y_nand, y_nor);
            if (y_inv  !== ~a)        errors = errors + 1;
            if (y_nand !== ~(a & b))  errors = errors + 1;
            if (y_nor  !== ~(a | b))  errors = errors + 1;
        end
        if (errors == 0) $display("PASS");
        else             $display("FAILED with %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
