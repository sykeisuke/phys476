module XOR1(
  input A,
  input B,
  output C
);

  assign C= ~(~(A & ~(B& B)) & (~(~(A & A) & B)));

endmodule

