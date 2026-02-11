`timescale 1ns/1ps

module SR4RE_tb;

  reg CLK;
  reg R;
  reg CE;
  reg SLI;
  wire [3:0] Q;

  SR4RE inst (
    .CLK(CLK),
    .R(R),
    .CE(CE),
    .SLI(SLI),
    .Q(Q)
  );

  // 100MHz CLK
  CLK = 1'b0;
  always #5 CLK = ~CLK;

  initial begin
    $display(" time(ns) | R CE SLI | Q");
    $monitor("%8t | %b  %b  %b  | %b", $time, R, CE, SLI, Q);

    // init
    R   = 1'b0;
    CE  = 1'b0;
    SLI = 1'b0;

    // Reset pulse
    #20;
    R = 1'b1;
    #40;
    R = 1'b0;

    // Enable shifting with SLI=1 (you should see Q shift in 1s)
    CE  = 1'b1;
    SLI = 1'b1;

    #2000;

    // Try changing serial input
    SLI = 1'b0;
    #2000;

    // Disable shifting
    CE = 1'b0;
    #2000;

    $finish;
  end

endmodule
