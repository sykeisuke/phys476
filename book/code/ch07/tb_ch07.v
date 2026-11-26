`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for Chapter 7: exhaustive for 4 bits,
// all 2^17 cases for the signed 8-bit add/subtract.
module tb_ch07;
    integer i, j, k, errors = 0;

    reg  a1, b1;
    wire hs, hc;
    half_adder u_ha (.a(a1), .b(b1), .s(hs), .c(hc));

    reg  fa_a, fa_b, fa_c;
    wire fs, fc;
    full_adder u_fa (.a(fa_a), .b(fa_b), .cin(fa_c), .s(fs), .cout(fc));

    reg  [3:0] a4, b4;
    reg        c4;
    wire [3:0] s_r, s_b;
    wire       co_r, co_b;
    rca       #(.N(4)) u_rca (.a(a4), .b(b4), .cin(c4), .s(s_r), .cout(co_r));
    add_behav #(.N(4)) u_add (.a(a4), .b(b4), .cin(c4), .s(s_b), .cout(co_b));

    reg  signed [7:0] sa, sb;
    reg               sub;
    wire signed [7:0] sy;
    wire              ovf;
    addsub #(.N(8)) u_as (.a(sa), .b(sb), .sub(sub), .y(sy), .ovf(ovf));

    reg  [11:0] adc, ped;
    wire signed [12:0] amp;
    pedestal_sub u_ps (.adc(adc), .pedestal(ped), .amp(amp));

    integer exact;
    initial begin
        for (i = 0; i < 4; i = i + 1) begin
            {a1, b1} = i[1:0]; #1;
            if ({hc, hs} !== a1 + b1) begin errors = errors + 1; $display("FAIL ha"); end
        end
        for (i = 0; i < 8; i = i + 1) begin
            {fa_a, fa_b, fa_c} = i[2:0]; #1;
            if ({fc, fs} !== fa_a + fa_b + fa_c) begin errors = errors + 1; $display("FAIL fa"); end
        end
        for (i = 0; i < 512; i = i + 1) begin
            {c4, a4, b4} = i[8:0]; #1;
            if ({co_r, s_r} !== a4 + b4 + c4 || {co_b, s_b} !== {co_r, s_r}) begin
                errors = errors + 1; $display("FAIL rca %d %d %d", a4, b4, c4);
            end
        end
        for (i = 0; i < 2 * 256 * 256; i = i + 1) begin
            {sub, sa, sb} = i[16:0]; #1;
            exact = sub ? (sa - sb) : (sa + sb);          // 32-bit integer math
            if (ovf !== (exact < -128 || exact > 127)) begin
                errors = errors + 1; $display("FAIL ovf %d %d %b", sa, sb, sub);
            end
            if (!ovf && sy !== exact[7:0]) begin
                errors = errors + 1; $display("FAIL addsub %d %d %b", sa, sb, sub);
            end
        end
        adc = 12'd100; ped = 12'd120; #1;
        if (amp !== -13'sd20) begin errors = errors + 1; $display("FAIL ped"); end
        adc = 12'd4095; ped = 12'd0; #1;
        if (amp !== 13'sd4095) begin errors = errors + 1; $display("FAIL ped max"); end
        $display("-20 in 8-bit two's complement: %b", -8'sd20);
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
