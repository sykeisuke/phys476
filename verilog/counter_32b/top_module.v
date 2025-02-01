module top_module(
  input CLK,
  input R,
  input CE,
  input reset,
  output LED0, 
  output LED1, 
  output LED2
);

wire clk_div;
wire locked;
wire [31:0] Q;

// Clocking Wizard instance
clk_wiz_0 clk_wiz_inst (
  .clk_out1(clk_div),
  .reset(reset),
  .locked(locked),
  .clk_in1(CLK) 
);

// Counter instance 
counter counter_inst (
  .CLK(clk_div),
  .R(R),
  .CE(CE),
  .Q(Q),
  .LED0(LED0),
  .LED1(LED1),
  .LED2(LED2)
);

endmodule
