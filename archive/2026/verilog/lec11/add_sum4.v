module adder_sum4 (
    input  [7:0] E0, E1, E2, E3,
    output [9:0] SUM
);
    assign SUM = E0 + E1 + E2 + E3;
endmodule

