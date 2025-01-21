module Half_Adder (
    input wire A,
    input wire B, 
    output wire S,    
    output wire COC
);

    // Sum is A XOR B
    assign S = A ^ B;

    // Carry Out is A AND B
    assign CO = A & B;

endmodule
