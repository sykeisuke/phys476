module Full_Adder (
    input wire A,
    input wire B,
    input wire CI,
    output wire S,
    output wire CO
);

    // Sum is A XOR B XOR CI
    assign S = A ^ B ^ CI;

    // Carry Out is (A AND B) OR (B AND CI) OR (A AND CI)
    assign CO = (A & B) | (B & CI) | (A & CI);

endmodule

