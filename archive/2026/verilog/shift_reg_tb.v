`timescale 1ns/1ps

module SR4RE_tb;

  reg CLK = 1'b0;
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
  always #5 CLK = ~CLK;

  initial begin

    R   = 1'b0;
    CE  = 1'b0;
    SLI = 1'b0;

    #20;
    R = 1'b1;
    #40;
    R = 1'b0;

    CE  = 1'b1;
    SLI = 1'b1;
    #2000;

    SLI = 1'b0;
    #2000;

    CE = 1'b0;
    #2000;

    $finish;
  end

endmodule
