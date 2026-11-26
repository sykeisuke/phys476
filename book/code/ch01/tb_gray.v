`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench: round trip for all 4096 codes, and
// consecutive Gray codes differ in exactly one bit.
module tb_gray;
    reg  [11:0] bin;
    wire [11:0] gray, back;
    reg  [11:0] prev;
    integer i, errors = 0;

    bin2gray #(.W(12)) u_b2g (.bin(bin), .gray(gray));
    gray2bin #(.W(12)) u_g2b (.gray(gray), .bin(back));

    // number of 1 bits
    function integer ones(input [11:0] v);
        integer k;
        begin
            ones = 0;
            for (k = 0; k < 12; k = k + 1)
                ones = ones + v[k];
        end
    endfunction

    initial begin
        for (i = 0; i < 4096; i = i + 1) begin
            bin = i;
            #1;
            if (back !== bin) begin
                $display("FAIL: round trip %0d -> %b -> %0d", bin, gray, back);
                errors = errors + 1;
            end
            if (i > 0 && ones(gray ^ prev) != 1) begin
                $display("FAIL: %0d -> %0d changes %0d bits", i - 1, i, ones(gray ^ prev));
                errors = errors + 1;
            end
            prev = gray;
        end
        // wrap-around 4095 -> 0 also changes one bit
        if (ones(prev ^ 12'd0) != 1) begin
            $display("FAIL: wrap-around");
            errors = errors + 1;
        end
        if (errors == 0) $display("PASS");
        else             $display("FAILED with %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
