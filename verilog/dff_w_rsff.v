module DFF(
  input D,
  input CLK,
  output reg Q,
  output reg nQ
);

  wire S1, R1, Q1, nQ1;
  wire S2, R2, Q2, nQ2;
  wire S3, R3;

  assign R1 = ~CLK;
  assign S1 = nQ2;
  assign Q1 = ~(R1 | nQ1);
  assign nQ1 = ~(S1 | Q1);

  assign R2 = ~D;
  assign S2 = ~CLK | Q1;
  assign Q2 = ~(R2 | nQ2);
  assign nQ2 = ~(S2 | Q2);

  assign R3 = nQ2;
  assign S3 = Q1;
  assign Q = ~(R3 | nQ);
  assign nQ = ~(S3 | Q);

endmodule

