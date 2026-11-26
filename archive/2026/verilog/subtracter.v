module subtracter(
  input A, 
  input B, 
  output BO, 
  output D
);

  assign {BO, D} = A - B;

endmodule

