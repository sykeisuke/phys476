module Full_Adder (
    input A,
    input B,
    input CI,
    output S,
    output CO
);

    // Sum is A XOR B XOR CI
    assign S = A ^ B ^ CI;

    // Carry Out is (A AND B) OR (B AND CI) OR (A AND CI)
    assign CO = (A & B) | (B & CI) | (A & CI);

endmodule

