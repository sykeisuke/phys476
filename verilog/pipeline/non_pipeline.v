module non_pipeline_simple #(
  parameter W = 16,
  parameter signed [W-1:0] K1 = 16'sd3,
  parameter signed [W-1:0] B1 = 16'sd5,
  parameter signed [W-1:0] K2 = 16'sd2,
  parameter signed [W-1:0] B2 = 16'sd7
)(
  input  wire                clk,
  input  wire                rst,
  input  wire                in_valid,
  input  wire signed [W-1:0] X,
  output reg                 out_valid,
  output reg  signed [W-1:0] Y
);

  wire signed [W-1:0] comb_y = ((X*K1 + B1)*K2 + B2);

  always @(posedge clk) begin
    if (rst) begin
      out_valid <= 1'b0;
      Y         <= '0;
    end else begin
      out_valid <= in_valid;      
      if (in_valid) Y <= comb_y;  
    end
  end

endmodule
