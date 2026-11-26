`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for full_sub (all 8 inputs) and
// sub3_signmag (all 64 input pairs).
module tb_sub;
    reg  a, b, bi;
    wire d, bo;
    reg  [2:0] x, y;
    wire [3:0] diff;
    wire       neg;
    wire [2:0] mag;
    integer i, j, errors = 0, exact;

    full_sub     u_fs (.a(a), .b(b), .bi(bi), .d(d), .bo(bo));
    sub3_signmag u_sm (.a(x), .b(y), .diff(diff), .neg(neg), .mag(mag));

    initial begin
        for (i = 0; i < 8; i = i + 1) begin
            {a, b, bi} = i[2:0];
            #1;
            // a - b - bi = d - 2*bo
            if ($signed({1'b0, a}) - b - bi !== $signed({1'b0, d}) - 2 * bo) begin
                $error("full_sub: a=%b b=%b bi=%b -> d=%b bo=%b", a, b, bi, d, bo);
                errors = errors + 1;
            end
        end
        for (i = 0; i < 8; i = i + 1)
            for (j = 0; j < 8; j = j + 1) begin
                x = i; y = j;
                #1;
                exact = i - j;
                if ($signed(diff) !== exact || neg !== (exact < 0) ||
                    mag !== ((exact < 0) ? -exact : exact)) begin
                    $error("sub3_signmag: %0d - %0d -> diff=%b neg=%b mag=%0d",
                           i, j, diff, neg, mag);
                    errors = errors + 1;
                end
            end
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
