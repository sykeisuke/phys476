`timescale 1ns/1ps

module tb_myproject;

  // Clock and reset
  reg ap_clk;
  reg ap_rst;
  reg ap_start;
  wire ap_done;
  wire ap_idle;
  wire ap_ready;

  // Input signals
  reg [47:0] fc1_input;
  reg fc1_input_ap_vld;

  // Output signals
  wire [15:0] layer13_out;
  wire layer13_out_ap_vld;

  // Clock generation
  initial ap_clk = 0;
  always #5 ap_clk = ~ap_clk;  // 100 MHz clock

  // Instantiate DUT
  myproject_0 uut (
    .ap_clk(ap_clk),
    .ap_rst(ap_rst),
    .ap_start(ap_start),
    .ap_done(ap_done),
    .ap_idle(ap_idle),
    .ap_ready(ap_ready),

    .fc1_input(fc1_input),
    .fc1_input_ap_vld(fc1_input_ap_vld),

    .layer13_out(layer13_out),
    .layer13_out_ap_vld(layer13_out_ap_vld)
  );

  initial begin
    // Initialization
    ap_rst = 1;
    ap_start = 0;
    fc1_input = 48'd0;
    fc1_input_ap_vld = 0;

    #20;
    ap_rst = 0;

    // Apply stimulus
    @(negedge ap_clk);
    fc1_input = 48'h0001_0002_0003;  // Replace with your input
    fc1_input_ap_vld = 1;
    ap_start = 1;

    @(negedge ap_clk);
    fc1_input_ap_vld = 0;
    ap_start = 0;

    // Wait for done signal
    wait (ap_done == 1);
    $display("Output: %h", layer13_out);

    #20;
    $finish;
  end

endmodule
