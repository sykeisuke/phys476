`timescale 1ns / 1ps

module tb_muladd;

  reg clk = 0;
  reg rst = 0;
  always #5 clk = ~clk;  // 100MHz

  reg [15:0] a [0:15];
  reg [15:0] b [0:15];
  wire [31:0] result;
  
  reg ap_start = 0;
  wire ap_done;
  wire ap_idle;
  wire ap_ready;

  wire [3:0] a_address0;
  wire       a_ce0;
  wire [15:0] a_q0;

  wire [3:0] b_address0;
  wire       b_ce0;
  wire [15:0] b_q0;

  wire [31:0] ap_return;

  muladd uut (
    .ap_clk(clk),
    .ap_rst(rst),
    .ap_start(ap_start),
    .ap_done(ap_done),
    .ap_idle(ap_idle),
    .ap_ready(ap_ready),
    .a_address0(a_address0),
    .a_ce0(a_ce0),
    .a_q0(a_q0),
    .b_address0(b_address0),
    .b_ce0(b_ce0),
    .b_q0(b_q0),
    .ap_return(ap_return)
  );

  assign a_q0 = (a_ce0) ? a[a_address0] : 16'd0;
  assign b_q0 = (b_ce0) ? b[b_address0] : 16'd0;
  assign result = ap_return;

  integer i;
  integer expected;

  initial begin
    $display("----- Start Simulation -----");

    expected = 0;
    for (i = 0; i < 16; i = i + 1) begin
      a[i] = i;
      b[i] = 2*i;
      expected = expected + a[i] * b[i];
    end

    rst = 1; #20;
    rst = 0;

    ap_start = 1;
    #10;
    ap_start = 0;

    wait (ap_done == 1);

    $display("Result: %d", result);
    $display("Expected: %d", expected);
    if (result == expected)
      $display("PASS");
    else
      $display("FAIL");

    $finish;
  end

endmodule
