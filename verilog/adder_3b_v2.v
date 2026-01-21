module adder_3b_v2 (
    input [2:0] A, 
    input [2:0] B, 
    output [2:0] S, 
    output CO
);

    assign {CO, S} = B + A;

endmodule

