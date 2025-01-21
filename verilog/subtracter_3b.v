module subtracter_3b(
  input [2:0] A, 
  input [2:0] B, 
  output BO, 
  output [2:0] D
);

  assign {BO, D} = A - B;

endmodule

