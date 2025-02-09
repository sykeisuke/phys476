module TIMER(
  input TRG_ONE, 
  input MODE, 
  input CLK, 
  input R, 
  output OUT
);

  parameter [7:0] N= 8'hFF;

  reg [7:0] counter_reg;
  reg out_reg;
  
  // DFF
  always @ (posedge CLK) begin
    if (R) begin
      out_reg <= 1'b0;
    end
    else if (counter_reg == N-1) begin
      out_reg <= 1'b0;
    end
    else if (TRG_ONE) begin
      out_reg <= 1'b1;
    end
  end

  // Counter
  always @ (posedge CLK) begin
    if (R== 1'b1) begin
      counter_reg <= 8'd0;
    end
    else if (counter_reg == N-1) begin
      counter_reg <= 8'd0;
    end
    else if (MODE && TRG_ONE) begin
      counter_reg <= 8'd0;
    end
    else if (out_reg) begin
      counter_reg <= counter_reg + 1'b1;
    end
  end

  assign OUT = out_reg;

endmodule

