`timescale 1ns / 1ps

module muladd_tb;

  parameter SIZE = 16;

  reg clk;
  reg rst;
  reg ap_start;

  wire ap_done;
  wire ap_idle;
  wire ap_ready;

  wire [3:0]  a_address0;
  wire        a_ce0;
  reg  [31:0] a_q0;

  wire [3:0]  b_address0;
  wire        b_ce0;
  reg  [31:0] b_q0;

  wire [31:0] result;

  reg [31:0] a_mem [0:SIZE-1];
  reg [31:0] b_mem [0:SIZE-1];

  reg [31:0] expected;
  integer i;

  // 100 MHz clock
  always #5 clk = ~clk;

  top dut (
      .clk(clk),
      .rst(rst),
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

      .result(result)
  );

  initial begin
    clk      = 1'b0;
    rst      = 1'b1;
    ap_start = 1'b0;
    a_q0     = 32'd0;
    b_q0     = 32'd0;
    expected = 32'd0;

    // initialize memories and expected result
    for (i = 0; i < SIZE; i = i + 1) begin
      a_mem[i] = i;
      b_mem[i] = i + 1;
      expected = expected + a_mem[i] * b_mem[i];
    end

    // release reset
    #20;
    rst = 1'b0;

    // start pulse: 1 clock
    @(posedge clk);
    ap_start = 1'b1;
    @(posedge clk);
    ap_start = 1'b0;
  end

  // memory read model
  always @(posedge clk) begin
    if (a_ce0)
      a_q0 <= a_mem[a_address0];
    if (b_ce0)
      b_q0 <= b_mem[b_address0];
  end

  // check result
  always @(posedge clk) begin
    if (ap_done) begin
      $display("Result   = %0d (0x%08h)", result, result);
      $display("Expected = %0d (0x%08h)", expected, expected);

      if (result == expected)
        $display("PASS");
      else
        $display("FAIL");

      #10;
      $finish;
    end
  end

endmodule
