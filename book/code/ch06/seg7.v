`timescale 1ns / 1ps
`default_nettype none

// Hexadecimal digit to seven-segment pattern.
// seg[0] = a, seg[1] = b, ..., seg[6] = g. A 1 turns the segment on.
// For a common-anode display (segments on when driven low), use ~seg.
module seg7 (
    input  wire [3:0] hex,
    output reg  [6:0] seg
);
    always @(*) begin
        case (hex)          //   gfedcba
            4'h0:    seg = 7'b0111111;
            4'h1:    seg = 7'b0000110;
            4'h2:    seg = 7'b1011011;
            4'h3:    seg = 7'b1001111;
            4'h4:    seg = 7'b1100110;
            4'h5:    seg = 7'b1101101;
            4'h6:    seg = 7'b1111101;
            4'h7:    seg = 7'b0000111;
            4'h8:    seg = 7'b1111111;
            4'h9:    seg = 7'b1101111;
            4'hA:    seg = 7'b1110111;
            4'hB:    seg = 7'b1111100;
            4'hC:    seg = 7'b0111001;
            4'hD:    seg = 7'b1011110;
            4'hE:    seg = 7'b1111001;
            default: seg = 7'b1110001;   // F
        endcase
    end
endmodule

`default_nettype wire
