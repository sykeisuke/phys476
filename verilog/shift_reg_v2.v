module SR4RE (
  input CLK,       
  input R,         
  input CE,        
  input SLI,       
  output reg [3:0] Q 
);

  reg [26:0] div_counter = 0;
  reg slow_clk = 0;           
  
  // Clock divider: Generates a slow clock from the 100MHz input clock
  always @(posedge CLK) begin
    div_counter <= div_counter + 1;
    slow_clk <= div_counter[24];
  end

  always @(posedge slow_clk) begin
    if (R) begin
      Q <= 4'b0000; 
    end
    else if (CE) begin
      Q <= {Q[2:0], SLI};
    end
  end

endmodule
