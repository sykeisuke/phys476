module tff (
  input CLK,
  input T,
  output reg Q
);

  always @(posedge CLK) begin
    if (T == 1'b1) begin
      Q <= ~Q;
    end
  end

endmodule

