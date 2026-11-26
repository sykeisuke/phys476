`timescale 1 ps / 1 ps

module top_module (
    input clk_100MHz,
    input reset_rtl_0,
    
    // FIFO write interface
    input [7:0] din_0,
    input wr_en_0,
    output full_0,
    
    // FIFO read interface
    output [7:0] dout_0,
    input rd_en_0,
    output empty_0
);

    // Internal signals
    wire clk_div;
    
    // Clocking Wizard Instance
    fifo_design_clk_wiz_0_1 clk_wiz_0_inst (
        .clk_in1(clk_100MHz),
        .clk_out1(clk_div),
        .reset(reset_rtl_0)
    );

    // FIFO Generator Instance
    fifo_design_fifo_generator_0_0 fifo_generator_0_inst (
        .clk(clk_div),
        .din(din_0),
        .wr_en(wr_en_0),
        .full(full_0),
        .rd_en(rd_en_0),
        .dout(dout_0),
        .empty(empty_0),
        .srst(reset_rtl_0)
    );

endmodule

