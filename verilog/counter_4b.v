module counter_4b(
  input CLK,
  input R,
  input CE,
  output reg [3:0] Q,
  output TC,
  output CEO
);

  always @(posedge CLK) begin
    if (R) begin
      Q <= 4'b0;
    end
    else if (CE) begin
      Q <= Q + 1'b1;
    end
  end

  assign TC = &Q;
  assign CEO = &Q & CE;

endmodule

