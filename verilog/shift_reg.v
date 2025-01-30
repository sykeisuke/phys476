module SR4RE (
  input CLK,
  input R,
  input CE,
  input SLI,
  output reg [3:0] Q
);

  always @(posedge CLK) begin
    if (R) begin
      Q <= 4'b0000
    end
    else if (CE) begin
      Q[0] <= SLI;
      Q[1] <= Q[0];
      Q[2] <= Q[1];
      Q[3] <= Q[2];
    end
  end

endmodule

