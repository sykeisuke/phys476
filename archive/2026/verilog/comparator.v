module COMP(
  input [1:0] A,
  input [1:0] B,
  output EQ
);

  assign EQ = (A == B) ? 1'b1 : 1'b0;

endmodule

