`timescale 1ns / 1ps
`default_nettype none

// Lab 4: one AXI4-Stream Data FIFO (Vivado IP axis_data_fifo_0:
// TDATA 1 byte, depth 512, one clock, no TLAST) between the ports.
// Vivado: add this file as a design source after generating the IP.
module top_module (
    input  wire       aclk,
    input  wire       aresetn,        // active low, as in all AXI IP
    // AXI4-Stream input (source -> FIFO)
    input  wire [7:0] s_axis_tdata,
    input  wire       s_axis_tvalid,
    output wire       s_axis_tready,
    // AXI4-Stream output (FIFO -> sink)
    output wire [7:0] m_axis_tdata,
    output wire       m_axis_tvalid,
    input  wire       m_axis_tready
);
    axis_data_fifo_0 u_fifo (
        .s_axis_aresetn(aresetn),
        .s_axis_aclk   (aclk),
        .s_axis_tvalid (s_axis_tvalid),
        .s_axis_tready (s_axis_tready),
        .s_axis_tdata  (s_axis_tdata),
        .m_axis_tvalid (m_axis_tvalid),
        .m_axis_tready (m_axis_tready),
        .m_axis_tdata  (m_axis_tdata));
endmodule

`default_nettype wire
