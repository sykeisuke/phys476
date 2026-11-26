module fde(
  input CLK,
  input CE,
  input D,
  output reg Q
);

  always @(posedge CLK) begin
    if (CE) begin
      Q <= D;
    end
  end

endmodule

