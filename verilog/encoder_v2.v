module encoder8to3 (
    input SW0,         // Input bit 0
    input SW1,         // Input bit 1
    input SW2,         // Input bit 2
    input SW3,         // Input bit 3
    input SW4,         // Input bit 4
    input SW5,         // Input bit 5
    input SW6,         // Input bit 6
    input SW7,         // Input bit 7
    output LED0,       // Output bit 0
    output LED1,       // Output bit 1
    output LED2        // Output bit 2
);

    // Combine SW inputs into a single 8-bit value
    wire [7:0] A = {SW7, SW6, SW5, SW4, SW3, SW2, SW1, SW0};

    // Perform the encoding
    assign {LED2, LED1, LED0} = (A[0] ? 3'b000 :
                                 (A[1] ? 3'b001 :
                                 (A[2] ? 3'b010 :
                                 (A[3] ? 3'b011 :
                                 (A[4] ? 3'b100 :
                                 (A[5] ? 3'b101 :
                                 (A[6] ? 3'b110 :
                                 (A[7] ? 3'b111 : 3'b000))))))));

endmodule
