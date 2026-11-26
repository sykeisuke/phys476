module RSFF (
  input S,
  input R,
  output Q,
  output Qbar
);

  assign Q = ~( S | Qbar );
  assign Qbar = ~( R | Q );

endmodule

