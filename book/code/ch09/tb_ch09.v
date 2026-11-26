`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for lut4 and tmr_vote.
module tb_ch09;
    integer i, k, errors = 0;

    // lut4 with the default INIT: y = (a & b) | (c & d)
    reg  a, b, c, d;
    wire y;
    lut4 u_lut (.a(a), .b(b), .c(c), .d(d), .y(y));

    // lut4 configured as a 4-input XOR (odd parity): INIT = 16'h6996
    wire yx;
    lut4 #(.INIT(16'h6996)) u_xor (.a(a), .b(b), .c(c), .d(d), .y(yx));

    // tmr_vote
    reg  [7:0] good, bad, va, vb, vc;
    wire [7:0] vy;
    wire       verr;
    tmr_vote #(.W(8)) u_tmr (.a(va), .b(vb), .c(vc), .y(vy), .err(verr));

    initial begin
        for (i = 0; i < 16; i = i + 1) begin
            {d, c, b, a} = i[3:0];
            #1;
            if (y !== ((a & b) | (c & d))) begin
                $display("FAIL: lut4 AND-OR at %b", i[3:0]);
                errors = errors + 1;
            end
            if (yx !== (a ^ b ^ c ^ d)) begin
                $display("FAIL: lut4 XOR at %b", i[3:0]);
                errors = errors + 1;
            end
        end

        // three equal copies: no error; one corrupted copy: outvoted and flagged
        for (i = 0; i < 256; i = i + 17) begin
            good = i;
            va = good; vb = good; vc = good;
            #1;
            if (vy !== good || verr !== 1'b0) begin
                $display("FAIL: tmr clean %h", good);
                errors = errors + 1;
            end
            for (k = 0; k < 3; k = k + 1) begin
                bad = good ^ (8'h01 << (i % 8));   // single-event upset in one bit
                va = (k == 0) ? bad : good;
                vb = (k == 1) ? bad : good;
                vc = (k == 2) ? bad : good;
                #1;
                if (vy !== good || verr !== 1'b1) begin
                    $display("FAIL: tmr upset in copy %0d of %h", k, good);
                    errors = errors + 1;
                end
            end
        end
        if (errors == 0) $display("PASS");
        else             $display("FAILED with %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
