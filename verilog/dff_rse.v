module dff_rse(
  input D,
  input CLK,
  input R,
  input S,
  input CE,
  output reg Q
);

  always @(posedge CLK) begin
    if (R) begin
      Q <= 1'b0; 
    end
    else if (S) begin
      Q <= 1'b1;
    end
    else if (CE) begin
      Q <= D;
    end
  end

endmodule

