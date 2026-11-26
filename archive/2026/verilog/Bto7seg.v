module Binary_to_7Segment (
    input [3:0] I,      // 4-bit binary input
    output [6:0] O      // 7-segment output (a-g)
);

    assign O = (I == 4'b0000) ? 7'b1111110 :   // 0
               (I == 4'b0001) ? 7'b0110000 :   // 1
               (I == 4'b0010) ? 7'b1101101 :   // 2
               (I == 4'b0011) ? 7'b1111001 :   // 3
               (I == 4'b0100) ? 7'b0110011 :   // 4
               (I == 4'b0101) ? 7'b1011011 :   // 5
               (I == 4'b0110) ? 7'b1011111 :   // 6
               (I == 4'b0111) ? 7'b1110000 :   // 7
               (I == 4'b1000) ? 7'b1111111 :   // 8
               (I == 4'b1001) ? 7'b1111011 :   // 9
                               7'b0000000;   // Default (blank display)

endmodule

