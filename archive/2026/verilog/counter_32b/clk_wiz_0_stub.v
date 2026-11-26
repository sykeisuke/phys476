module clk_wiz_0(clk_out1, reset, locked, clk_in1)
/ synthesis syn_black_box black_box_pad_pin="reset,locked,clk_in1" /
/ synthesis syn_force_seq_prim="clk_out1" /;
  output clk_out1 / synthesis syn_isclock = 1 /;
  input reset;
  output locked;
  input clk_in1;
endmodule
