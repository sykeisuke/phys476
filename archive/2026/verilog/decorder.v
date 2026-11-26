module Decoder (
  input A0, 
  input A1, 
  input E,
  output D0, 
  output D1, 
  output D2, 
  output D3
);

  assign D0 = E & ~A1 & ~A0;  // 00
  assign D1 = E & ~A1 & A0;   // 01
  assign D2 = E & A1 & ~A0;   // 10
  assign D3 = E & A1 & A0;    // 11

endmodule

