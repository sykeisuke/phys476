module XOR4(
  input A,
  input B,
  output C
);

  XOR0 xor_instance (
    .IO(A), 
    .I1(B), 
    .O(C)
  );

endmodule

module XOR0(
  input I0,
  input I1,
  output O
);

  assign O = I0 ^ I1;

endmodule

