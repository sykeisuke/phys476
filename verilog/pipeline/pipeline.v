module pipeline_simple #(
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

  reg signed [W-1:0] s1, s2, s3;
  reg v1, v2, v3;

  always @(posedge clk) begin
    if (rst) begin
      v1 <= 0; v2 <= 0; v3 <= 0;
      s1 <= '0; s2 <= '0; s3 <= '0;
      out_valid <= 0;
      Y <= '0;
    end else begin
      v1 <= in_valid;
      v2 <= v1;
      v3 <= v2;
      out_valid <= v3;

      // Stage1: X*K1
      if (in_valid) s1 <= X*K1;

      // Stage2: (s1+B1)*K2
      if (v1) s2 <= (s1 + B1)*K2;

      // Stage3: s2+B2
      if (v2) s3 <= s2 + B2;

      // Output
      if (v3) Y <= s3;
    end
  end

endmodule
