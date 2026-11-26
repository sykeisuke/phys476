module MG_COMP(
  input [1:0] A,
  input [1:0] B,
  output GT,
  output LT
);

  assign GT = (A > B) ? 1'b1 : 1'b0;
  assign LT = (A < B) ? 1'b1 : 1'b0;

endmodule

