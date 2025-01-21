module Full_Adder (
    input A,
    input B,
    input CI,
    output S,
    output CO
);

    // Internal wires for Half Adder outputs
    wire S1;
    wire C1, C2;

    // First Half Adder: adds A and B
    assign S1 = A ^ B;
    assign C1 = A & B;

    // Second Half Adder: adds S1 and CI
    assign S = S1 ^ CI;
    assign C2 = S1 & CI;

    // OR gate for Carry Out
    assign CO = C1 | C2;

endmodule

