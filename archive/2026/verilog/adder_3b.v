module adder_3b (
    input A0, 
    input A1, 
    input A2,      
    input B0, 
    input B1, 
    input B2,      
    output S0, 
    output S1, 
    output S2,
    output CO
);

    assign {CO, S2, S1, S0} = {B2, B1, B0} + {A2, A1, A0};

endmodule

