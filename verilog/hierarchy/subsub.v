// leaf_A.v
module leaf_A(input wire x, output wire y);
    assign y = ~x;
endmodule

// leaf_B.v
module leaf_B(input wire x, output wire y);
    assign y = x & 1'b1;
endmodule

// leaf_C.v
module leaf_C(input wire x, output wire y);
    assign y = x | 1'b0;
endmodule
