`timescale 1ns / 1ps
`default_nettype none

// Icarus-only stand-in for the Vivado FIFO Generator in Lab 2, so that
// tb_fifo_ip.v can be tried without Vivado. It wraps our sync_fifo with
// depth 16. Do NOT add this file to the Vivado project: there the real
// fifo_generator_0 from the IP Catalog takes its place.
module fifo_generator_0 (
    input  wire       clk,
    input  wire       srst,
    input  wire [7:0] din,
    input  wire       wr_en,
    input  wire       rd_en,
    output wire [7:0] dout,
    output wire       full,
    output wire       empty
);
    sync_fifo #(.WIDTH(8), .AW(4)) u_fifo (
        .clk(clk), .rst(srst), .wr_en(wr_en), .din(din),
        .rd_en(rd_en), .dout(dout), .full(full), .empty(empty));
endmodule

`default_nettype wire
