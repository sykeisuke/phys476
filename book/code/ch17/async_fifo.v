`timescale 1ns / 1ps
`default_nettype none

// Asynchronous FIFO: writer on wclk, reader on rclk (unrelated clocks).
// Pointers have one extra bit (full vs empty) and cross the domains as
// Gray code through two flip-flops, so a synchronized pointer is always
// an old or the new value, never a mix. DEPTH = 2**AW words, AW >= 2.
module async_fifo #(
    parameter W  = 8,
    parameter AW = 4
) (
    input  wire         wclk,
    input  wire         wrst,        // synchronous to wclk, active high
    input  wire         wr_en,
    input  wire [W-1:0] din,
    output reg          full,
    input  wire         rclk,
    input  wire         rrst,        // synchronous to rclk, active high
    input  wire         rd_en,
    output reg  [W-1:0] dout,        // valid one rclk after rd_en
    output reg          empty
);
    reg [W-1:0] mem [0:(1<<AW)-1];
    reg [AW:0]  wbin, wgray;         // write pointer (wclk domain)
    reg [AW:0]  rbin, rgray;         // read pointer (rclk domain)

    // ---------------------------------------------------- write domain
    (* ASYNC_REG = "TRUE" *) reg [AW:0] rg1, rg2;   // read pointer, Gray
    wire        do_wr   = wr_en && !full;
    wire [AW:0] wbin_n  = wbin + {{AW{1'b0}}, do_wr};
    wire [AW:0] wgray_n = (wbin_n >> 1) ^ wbin_n;
    // full: next write pointer is one lap ahead of the read pointer
    wire        full_n  = (wgray_n == {~rg2[AW:AW-1], rg2[AW-2:0]});

    always @(posedge wclk) begin
        if (wrst) begin
            wbin <= 0;  wgray <= 0;  full <= 1'b0;
            rg1  <= 0;  rg2   <= 0;
        end else begin
            wbin  <= wbin_n;
            wgray <= wgray_n;
            full  <= full_n;
            rg1   <= rgray;
            rg2   <= rg1;
            if (do_wr)
                mem[wbin[AW-1:0]] <= din;
        end
    end

    // ---------------------------------------------------- read domain
    (* ASYNC_REG = "TRUE" *) reg [AW:0] wg1, wg2;   // write pointer, Gray
    wire        do_rd   = rd_en && !empty;
    wire [AW:0] rbin_n  = rbin + {{AW{1'b0}}, do_rd};
    wire [AW:0] rgray_n = (rbin_n >> 1) ^ rbin_n;
    // empty: next read pointer has caught up with the write pointer
    wire        empty_n = (rgray_n == wg2);

    always @(posedge rclk) begin
        if (rrst) begin
            rbin <= 0;  rgray <= 0;  empty <= 1'b1;
            wg1  <= 0;  wg2   <= 0;
        end else begin
            rbin  <= rbin_n;
            rgray <= rgray_n;
            empty <= empty_n;
            wg1   <= wgray;
            wg2   <= wg1;
            if (do_rd)
                dout <= mem[rbin[AW-1:0]];
        end
    end
endmodule

`default_nettype wire
