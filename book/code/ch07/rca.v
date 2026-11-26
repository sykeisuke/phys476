`timescale 1ns / 1ps
`default_nettype none

// N-bit ripple-carry adder built from full adders.
module rca #(
    parameter N = 4
) (
    input  wire [N-1:0] a, b,
    input  wire         cin,
    output wire [N-1:0] s,
    output wire         cout
);
    wire [N:0] c;              // c[k] is the carry into bit k
    assign c[0] = cin;

    genvar k;
    generate
        for (k = 0; k < N; k = k + 1) begin : bit_slice
            full_adder fa (.a(a[k]), .b(b[k]), .cin(c[k]),
                           .s(s[k]), .cout(c[k+1]));
        end
    endgenerate

    assign cout = c[N];
endmodule

`default_nettype wire
