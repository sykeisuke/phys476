module counter(
  input CLK,
  input R,
  input CE,  
  output reg [31:0] Q,
  output LED0,
  output LED1,
  output LED2
);

  always @(posedge CLK) begin
    if (R) begin
      Q <= 32'b0;
    end
    else if (CE) begin
      Q <= Q + 1'b1;
    end
  end
  
  assign LED0 = Q[28];
  assign LED1 = Q[29]; 
  assign LED2 = Q[30]; 

endmodule
