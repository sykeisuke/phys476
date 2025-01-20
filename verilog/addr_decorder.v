module Address_Decoder (
    input  [2:0] ADDR,          // 3-bit address input
    output [7:0] MODULE_SEL     // 8-bit module select output
);

    // Decode the address
    assign MODULE_SEL = (ADDR == 3'b000) ? 8'b00000001 :   // Module 0
                        (ADDR == 3'b001) ? 8'b00000010 :   // Module 1
                        (ADDR == 3'b010) ? 8'b00000100 :   // Module 2
                        (ADDR == 3'b011) ? 8'b00001000 :   // Module 3
                        (ADDR == 3'b100) ? 8'b00010000 :   // Module 4
                        (ADDR == 3'b101) ? 8'b00100000 :   // Module 5
                        (ADDR == 3'b110) ? 8'b01000000 :   // Module 6
                        (ADDR == 3'b111) ? 8'b10000000 :   // Module 7
                                          8'b00000000;    // Default (none selected)

endmodule
