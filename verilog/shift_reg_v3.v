module SR4RE (
  input CLK,       
  input R,         
  input CE,        
  input SLI,       
  output reg [3:0] Q 
);

  reg [27:0] div_counter = 0;
  reg tick = 0;           
  
  always @(posedge CLK) begin
    div_counter <= div_counter + 1;
    tick <= (div_counter[27] == 1'b1) && (div_counter[26:0] == 27'h0);

    if (R) begin
      Q <= 4'b0000; 
    end
    else if (CE && tick) begin
      Q <= {Q[2:0], SLI};
    end

  end

endmodule
