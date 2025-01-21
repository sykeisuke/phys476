module Half_Adder (
    input A,
    input B, 
    output S,    
    output CO
);

    // Sum is A XOR B
    assign S = A ^ B;

    // Carry Out is A AND B
    assign CO = A & B;

endmodule
