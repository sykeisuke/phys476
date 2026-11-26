`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for the Chapter 6 blocks.
module tb_ch06;
    integer i, j, errors = 0;

    // mux2
    reg  d0, d1, s;
    wire yg, y;
    mux2 u_mux (.d0(d0), .d1(d1), .s(s), .y_gates(yg), .y(y));

    // dec2to4
    reg  [1:0] a;
    reg        en;
    wire [3:0] d;
    dec2to4 u_dec (.a(a), .en(en), .d(d));

    // prio_enc8
    reg  [7:0] pin;
    wire [2:0] pidx;
    wire       pval;
    prio_enc8 u_pe (.in(pin), .idx(pidx), .valid(pval));

    // pixel_encoder
    reg  [63:0] hit;
    wire [5:0]  xidx;
    wire        xval;
    pixel_encoder u_px (.hit(hit), .idx(xidx), .valid(xval));

    // seg7
    reg  [3:0] hex;
    wire [6:0] seg;
    seg7 u_seg (.hex(hex), .seg(seg));

    // cmp2
    reg  [1:0] ca, cb;
    wire ceqg, ceq;
    cmp2 u_cmp (.a(ca), .b(cb), .eq_gates(ceqg), .eq(ceq));

    // energy_trigger
    reg  [9:0]  e0, e1, e2, e3;
    reg  [11:0] thr;
    wire [11:0] esum;
    wire        trig;
    energy_trigger u_et (.e0(e0), .e1(e1), .e2(e2), .e3(e3), .threshold(thr),
                         .esum(esum), .trig(trig));

    task check(input cond, input [8*24-1:0] what);
        if (!cond) begin
            $display("FAIL: %0s", what);
            errors = errors + 1;
        end
    endtask

    // reference: highest set bit of an 8-bit value
    function [2:0] msb8(input [7:0] v);
        integer k;
        begin
            msb8 = 0;
            for (k = 0; k < 8; k = k + 1) if (v[k]) msb8 = k;
        end
    endfunction

    integer k, ones;
    initial begin
        // mux2: all 8 input combinations
        for (i = 0; i < 8; i = i + 1) begin
            {s, d1, d0} = i[2:0]; #1;
            check(yg === y && y === (s ? d1 : d0), "mux2");
        end
        // dec2to4
        for (i = 0; i < 8; i = i + 1) begin
            {en, a} = i[2:0]; #1;
            check(d === (en ? (4'b1 << a) : 4'b0), "dec2to4");
        end
        // prio_enc8: all 256 inputs
        for (i = 0; i < 256; i = i + 1) begin
            pin = i[7:0]; #1;
            check(pval === (pin != 0), "prio_enc8 valid");
            check(pidx === (pin == 0 ? 3'd0 : msb8(pin)), "prio_enc8 idx");
        end
        // pixel_encoder: single hits, no hit, the double hit of the text
        hit = 64'b0; #1;
        check(xval === 1'b0 && xidx === 6'd0, "pixel none");
        for (i = 0; i < 64; i = i + 1) begin
            hit = 64'b1 << i; #1;
            check(xval === 1'b1 && xidx === i[5:0], "pixel single");
        end
        hit = (64'b1 << 27) | (64'b1 << 36); #1;
        check(xidx === 6'd27, "pixel double");
        // seg7: number of lit segments per digit
        for (i = 0; i < 16; i = i + 1) begin
            hex = i[3:0]; #1;
            ones = 0;
            for (k = 0; k < 7; k = k + 1) ones = ones + seg[k];
            case (i)
                0, 6, 9, 10:         check(ones == 6, "seg7 count");
                1:                   check(ones == 2, "seg7 count");
                2, 3, 5, 11, 13, 14: check(ones == 5, "seg7 count");
                4, 12, 15:           check(ones == 4, "seg7 count");
                7:                   check(ones == 3, "seg7 count");
                8:                   check(ones == 7, "seg7 count");
            endcase
            $display("seg7 %h -> %b", hex, seg);
        end
        // cmp2: all 16 combinations
        for (i = 0; i < 16; i = i + 1) begin
            {ca, cb} = i[3:0]; #1;
            check(ceqg === ceq && ceq === (ca == cb), "cmp2");
        end
        // energy_trigger: largest inputs, and around the threshold
        e0 = 1023; e1 = 1023; e2 = 1023; e3 = 1023; thr = 12'd4000; #1;
        check(esum === 12'd4092 && trig === 1'b1, "esum max");
        e0 = 100; e1 = 200; e2 = 300; e3 = 400; thr = 12'd1000; #1;
        check(esum === 12'd1000 && trig === 1'b0, "esum equal");
        thr = 12'd999; #1;
        check(trig === 1'b1, "esum above");

        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
