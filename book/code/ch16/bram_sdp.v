`timescale 1ns / 1ps
`default_nettype none

// Simple dual-port RAM: one write port, one read port, one clock.
// The read is synchronous (dout is a register), so Vivado can map it
// to block RAM. Read latency: 1 clock.
module bram_sdp #(
    parameter WIDTH = 16,
    parameter AW    = 10             // 1024 words x 16 bits = 16 Kb
) (
    input  wire             clk,
    input  wire             we,
    input  wire [AW-1:0]    waddr,
    input  wire [WIDTH-1:0] din,
    input  wire [AW-1:0]    raddr,
    output reg  [WIDTH-1:0] dout
);
    (* ram_style = "block" *) reg [WIDTH-1:0] mem [0:(1<<AW)-1];

    always @(posedge clk) begin
        if (we)
            mem[waddr] <= din;
        dout <= mem[raddr];
    end
endmodule

`default_nettype wire
