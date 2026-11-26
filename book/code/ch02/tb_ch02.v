`timescale 1ns / 1ps
`default_nettype none

// Prints the truth tables of all gates and checks the NAND-only circuits.
module tb_ch02;
    reg a, b;
    wire y_not, y_and, y_or, y_xor, y_nand, y_nor, y_xnor;
    wire n_not, n_and, n_or;
    integer i, errors = 0;

    gates     u_g (.a(a), .b(b), .y_not(y_not), .y_and(y_and), .y_or(y_or),
                   .y_xor(y_xor), .y_nand(y_nand), .y_nor(y_nor), .y_xnor(y_xnor));
    nand_only u_n (.a(a), .b(b), .y_not(n_not), .y_and(n_and), .y_or(n_or));

    initial begin
        $display(" a b | NOT AND OR XOR NAND NOR XNOR");
        for (i = 0; i < 4; i = i + 1) begin
            {a, b} = i[1:0];
            #1;
            $display(" %b %b |  %b   %b   %b   %b    %b    %b    %b",
                     a, b, y_not, y_and, y_or, y_xor, y_nand, y_nor, y_xnor);
            if (n_not !== y_not || n_and !== y_and || n_or !== y_or) begin
                $error("NAND-only mismatch at a=%b b=%b", a, b);
                errors = errors + 1;
            end
        end
        if (errors == 0) $display("PASS");
        $finish;
    end
endmodule

`default_nettype wire
