module complementer (
  input A0,
  input A1,
  input A2,
  input B0,
  input B1,
  input B2,
  output D0,
  output D1,
  output D2,
  output BO
);

  wire [3:0] sub;

  assign sub = {1'b0, A2, A1, A0} - {1'b0, B2, B1, B0};
  assign BO = sub[3];
  assign {D2, D1, D0} = sub[3] ? (~sub[2:0] + 1'b1) : sub[2:0]

endmodule

