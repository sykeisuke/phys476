`timescale 1ns / 1ps

module counter_tb;

  reg CLK;
  reg R;
  reg CE;
  wire [31:0] Q;
  wire LED0, LED1, LED2;

  // 100MHz CLK Generation
  initial begin
    CLK = 1'b0;
  end
  always #100 CLK = ~CLK; // 200ns period (5MHz clock)

  // Binary Counter instance
  counter counter_inst (
    .CLK(CLK),
    .R(R),
    .CE(CE),
    .Q(Q),
    .LED0(LED0),
    .LED1(LED1),
    .LED2(LED2)
  );

  // Simulation
  initial begin
    R = 1'b1;
    CE = 1'b0;
    
    repeat (20) @(posedge CLK);
    R = 1'b0;

    repeat (20) @(posedge CLK);
    CE = 1'b1;

    repeat (1000) @(posedge CLK);

    $finish;
  end
endmodule
