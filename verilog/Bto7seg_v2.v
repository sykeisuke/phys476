module Binary_to_7Segment (
    input SW0,         // Input bit 0
    input SW1,         // Input bit 1
    input SW2,         // Input bit 2
    input SW3,         // Input bit 3
    output a,          // Segment a
    output b,          // Segment b
    output c,          // Segment c
    output d,          // Segment d
    output e,          // Segment e
    output f,          // Segment f
    output g           // Segment g
);

    // Combine inputs into a single 4-bit value
    wire [3:0] I = {SW3, SW2, SW1, SW0};

    // Assign each segment output based on the 4-bit input
    assign {a, b, c, d, e, f, g} = (I == 4'b0000) ? 7'b1111110 : // 0
                                   (I == 4'b0001) ? 7'b0110000 : // 1
                                   (I == 4'b0010) ? 7'b1101101 : // 2
                                   (I == 4'b0011) ? 7'b1111001 : // 3
                                   (I == 4'b0100) ? 7'b0110011 : // 4
                                   (I == 4'b0101) ? 7'b1011011 : // 5
                                   (I == 4'b0110) ? 7'b1011111 : // 6
                                   (I == 4'b0111) ? 7'b1110000 : // 7
                                   (I == 4'b1000) ? 7'b1111111 : // 8
                                   (I == 4'b1001) ? 7'b1111011 : // 9
                                                    7'b0000000;  // Default (blank display)

endmodule
