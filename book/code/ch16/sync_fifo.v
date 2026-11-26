`timescale 1ns / 1ps
`default_nettype none

// Synchronous FIFO (one clock). The pointers have one extra bit:
// equal pointers mean empty; equal addresses with different extra
// bits mean full (the writer is one lap ahead of the reader).
module sync_fifo #(
    parameter WIDTH = 8,
    parameter AW    = 3              // DEPTH = 2**AW
) (
    input  wire             clk,
    input  wire             rst,     // synchronous, active high
    input  wire             wr_en,
    input  wire [WIDTH-1:0] din,
    input  wire             rd_en,
    output reg  [WIDTH-1:0] dout,
    output wire             full,
    output wire             empty
);
    reg [WIDTH-1:0] mem [0:(1<<AW)-1];
    reg [AW:0] wp, rp;               // AW+1 bits

    assign empty = (wp == rp);
    assign full  = (wp[AW] != rp[AW]) && (wp[AW-1:0] == rp[AW-1:0]);

    wire do_wr = wr_en && !full;
    wire do_rd = rd_en && !empty;

    always @(posedge clk) begin
        if (rst) begin
            wp <= 0;
            rp <= 0;
        end else begin
            if (do_wr) begin
                mem[wp[AW-1:0]] <= din;
                wp <= wp + 1'b1;
            end
            if (do_rd) begin
                dout <= mem[rp[AW-1:0]];
                rp <= rp + 1'b1;
            end
        end
    end
endmodule

`default_nettype wire
