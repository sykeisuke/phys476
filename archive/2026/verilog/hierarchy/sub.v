// sub_module.v
module sub_module(
    input  wire in,
    output wire out
);
    wire w1, w2, w3;

    leaf_A uA(.x(in), .y(w1));
    leaf_B uB(.x(in), .y(w2));
    leaf_C uC(.x(in), .y(w3));

    assign out = w1 ^ w2 ^ w3;
endmodule
