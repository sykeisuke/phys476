`timescale 1ns / 1ps

module muladd_tb;

  // Parameters
  parameter SIZE = 16;

  // Clock and reset
  reg clk;
  reg rst;

  // Control signals
  reg ap_start;
  wire ap_done;
  wire ap_idle;
  wire ap_ready;

  // Inputs
  reg [15:0] a[0:SIZE-1];
  reg [15:0] b[0:SIZE-1];
  wire [3:0] a_address0;
  wire [3:0] b_address0;
  wire a_ce0;
  wire b_ce0;
  reg [15:0] a_q0;
  reg [15:0] b_q0;

  // Output
  wire [31:0] ap_return;

  // Reference expected result
  reg [31:0] expected;
  reg [31:0] result;

  // Instantiate the DUT
  muladd_0 dut (
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

  // Clock generation
  always #5 clk = ~clk;

  // Test logic
  initial begin
    integer i;
    clk = 0;
    rst = 1;
    ap_start = 0;
    expected = 0;

    // Wait for a few cycles
    #20;
    rst = 0;

    // Initialize input data
    for (i = 0; i < SIZE; i = i + 1) begin
      a[i] = i;       // 0 to 15
      b[i] = i + 1;   // 1 to 16
      expected = expected + a[i] * b[i];
    end

    // Wait for DUT to be ready
    #20;

    ap_start = 1;
    #10;
    ap_start = 0;

    // Feed memory values to DUT
    while (!ap_done) begin
      #10;
      if (a_ce0)
        a_q0 = a[a_address0];
      if (b_ce0)
        b_q0 = b[b_address0];
    end

    // Get result
    result = ap_return;
    #10;
    $display("Result = 0x%08x", result);
    $display("Expected = 0x%08x", expected);

    if (result == expected) begin
      $display("PASS");
    end else begin
      $display("FAIL");
    end

    $stop;
  end

endmodule
