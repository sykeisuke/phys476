`timescale 1ns / 1ps
`default_nettype none

// Binary <-> Gray code conversion, as used in the counter of a Wilkinson ADC.
module bin2gray #(parameter W = 12) (
    input  wire [W-1:0] bin,
    output wire [W-1:0] gray
);
    assign gray = bin ^ (bin >> 1);
endmodule

module gray2bin #(parameter W = 12) (
    input  wire [W-1:0] gray,
    output reg  [W-1:0] bin
);
    integer i;
    always @(*) begin
        bin[W-1] = gray[W-1];
        for (i = W-2; i >= 0; i = i - 1)
            bin[i] = bin[i+1] ^ gray[i];
    end
endmodule

`default_nettype wire
