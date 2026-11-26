`timescale 1ns/1ps

module pipeline_tb;

  reg clk = 0;
  always #5 clk = ~clk;

  reg rst;
  reg in_valid;
  reg signed [15:0] X;

  wire out_valid_np;
  wire signed [15:0] Y_np;

  wire out_valid_pl;
  wire signed [15:0] Y_pl;

  non_pipeline u_np (
    .clk(clk), 
    .rst(rst),
    .in_valid(in_valid), 
    .X(X),
    .out_valid(out_valid_np), 
    .Y(Y_np)
  );

  pipeline u_pl (
    .clk(clk), 
    .rst(rst),
    .in_valid(in_valid), 
    .X(X),
    .out_valid(out_valid_pl), 
    .Y(Y_pl)
  );

  integer i;

  initial begin
    rst = 1;
    in_valid = 0;
    X = 0;

    // reset for a few cycles
    repeat (3) @(posedge clk);
    rst <= 0;

    // X: -3, -2, -1, 0, 1, 2, 3
    for (i = -3; i <= 3; i = i + 1) begin
      @(posedge clk);
      in_valid <= 1;
      X <= i;

      // insert a gap after X=-1 just for waveform clarity
      if (i == -1) begin
        @(posedge clk);
        in_valid <= 0;
        X <= 0;
      end
    end

    // stop driving
    @(posedge clk);
    in_valid <= 0;
    X <= 0;

    // wait so you can see pipeline outputs settle
    repeat (10) @(posedge clk);

    $finish;
  end

endmodule
