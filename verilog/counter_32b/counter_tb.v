`timescale 1ns / 1ps

module counter_tb;

  reg CLK;
  reg R;
  reg CE;
  reg reset;
  wire [31:0] Q;
  wire LED0, LED1, LED2;
  wire clk_div;
  wire locked;

  // 100MHz CLK Generation
  initial begin
    CLK = 1'b0;
  end
  always #5 CLK = ~CLK; // 10ns period (100MHz clock)

  // Clocking Wizard instance
  clk_wiz_0 clk_wiz_inst (
    .clk_out1(clk_div),
    .reset(reset),  
    .locked(locked),
    .clk_in1(CLK)
  );

  // Binary Counter instance
  counter counter_inst (
    .CLK(clk_div),
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
    reset = 1'b1; 
    
    repeat (10) @(posedge CLK); 
    reset = 1'b0;

    wait(locked == 1); 

    repeat (20) @(posedge CLK);
    R = 1'b0;

    repeat (20) @(posedge CLK);
    CE = 1'b1;

    repeat (1000) @(posedge clk_div);

    $finish;
  end
endmodule
