//Copyright 1986-2022 Xilinx, Inc. All Rights Reserved.
//Copyright 2022-2023 Advanced Micro Devices, Inc. All Rights Reserved.
//--------------------------------------------------------------------------------
//Tool Version: Vivado v.2023.2 (win64) Build 4029153 Fri Oct 13 20:14:34 MDT 2023
//Date        : Mon Feb 10 18:24:13 2025
//Host        : ykeisuke running 64-bit major release  (build 9200)
//Command     : generate_target fifo_design.bd
//Design      : fifo_design
//Purpose     : IP block netlist
//--------------------------------------------------------------------------------
`timescale 1 ps / 1 ps

( CORE_GENERATION_INFO = "fifo_design,IP_Integrator,{x_ipVendor=xilinx.com,x_ipLibrary=BlockDiagram,x_ipName=fifo_design,x_ipVersion=1.00.a,x_ipLanguage=VERILOG,numBlks=2,numReposBlks=2,numNonXlnxBlks=0,numHierBlks=0,maxHierDepth=0,numSysgenBlks=0,numHlsBlks=0,numHdlrefBlks=0,numPkgbdBlks=0,bdsource=USER,da_board_cnt=4,da_clkrst_cnt=1,synth_mode=None}" ) ( HW_HANDOFF = "fifo_design.hwdef" ) 
module fifo_design
   (clk_100MHz,
    din_0,
    dout_0,
    empty_0,
    full_0,
    rd_en_0,
    reset_rtl_0,
    wr_en_0);
  ( X_INTERFACE_INFO = "xilinx.com:signal:clock:1.0 CLK.CLK_100MHZ CLK" ) ( X_INTERFACE_PARAMETER = "XIL_INTERFACENAME CLK.CLK_100MHZ, CLK_DOMAIN fifo_design_clk_100MHz, FREQ_HZ 100000000, FREQ_TOLERANCE_HZ 0, INSERT_VIP 0, PHASE 0.0" ) input clk_100MHz;
  input [17:0]din_0;
  output [17:0]dout_0;
  output empty_0;
  output full_0;
  input rd_en_0;
  ( X_INTERFACE_INFO = "xilinx.com:signal:reset:1.0 RST.RESET_RTL_0 RST" ) ( X_INTERFACE_PARAMETER = "XIL_INTERFACENAME RST.RESET_RTL_0, INSERT_VIP 0, POLARITY ACTIVE_HIGH" ) input reset_rtl_0;
  input wr_en_0;

  wire clk_100MHz_1;
  wire clk_wiz_0_clk_out1;
  wire [17:0]din_0_1;
  wire [17:0]fifo_generator_0_dout;
  wire fifo_generator_0_empty;
  wire fifo_generator_0_full;
  wire rd_en_0_1;
  wire reset_rtl_0_1;
  wire wr_en_0_1;

  assign clk_100MHz_1 = clk_100MHz;
  assign din_0_1 = din_0[17:0];
  assign dout_0[17:0] = fifo_generator_0_dout;
  assign empty_0 = fifo_generator_0_empty;
  assign full_0 = fifo_generator_0_full;
  assign rd_en_0_1 = rd_en_0;
  assign reset_rtl_0_1 = reset_rtl_0;
  assign wr_en_0_1 = wr_en_0;
  fifo_design_clk_wiz_0_1 clk_wiz_0
       (.clk_in1(clk_100MHz_1),
        .clk_out1(clk_wiz_0_clk_out1),
        .reset(reset_rtl_0_1));
  fifo_design_fifo_generator_0_0 fifo_generator_0
       (.clk(clk_wiz_0_clk_out1),
        .din(din_0_1),
        .dout(fifo_generator_0_dout),
        .empty(fifo_generator_0_empty),
        .full(fifo_generator_0_full),
        .rd_en(rd_en_0_1),
        .srst(reset_rtl_0_1),
        .wr_en(wr_en_0_1));
endmodule

