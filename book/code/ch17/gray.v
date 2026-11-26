`timescale 1ns / 1ps
`default_nettype none

// Binary to Gray code: neighbouring values differ in exactly one bit.
module bin2gray #(parameter N = 4) (
    input  wire [N-1:0] bin,
    output wire [N-1:0] gray
);
    assign gray = bin ^ (bin >> 1);
endmodule

// Gray code back to binary: each binary bit is the XOR of all
// Gray bits at and above it.
module gray2bin #(parameter N = 4) (
    input  wire [N-1:0] gray,
    output reg  [N-1:0] bin
);
    integer i;
    always @(*) begin
        bin[N-1] = gray[N-1];
        for (i = N-2; i >= 0; i = i - 1)
            bin[i] = bin[i+1] ^ gray[i];
    end
endmodule

`default_nettype wire
