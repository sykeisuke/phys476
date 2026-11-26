`timescale 1ns / 1ps
`default_nettype none

// y = a*b + c, NOT pipelined: one long combinational path
// between the input registers and the output register.
// Latency 2 cycles (input reg + output reg), one result per cycle,
// but the clock period must cover the multiplier AND the adder.
module mac_comb (
    input  wire        clk,
    input  wire        in_valid,
    input  wire [7:0]  a, b,
    input  wire [15:0] c,
    output reg         out_valid,
    output reg  [16:0] y
);
    reg [7:0]  a_r, b_r;
    reg [15:0] c_r;
    reg        v_r;

    initial begin
        v_r = 1'b0;
        out_valid = 1'b0;
    end

    always @(posedge clk) begin
        a_r <= a;
        b_r <= b;
        c_r <= c;
        v_r <= in_valid;
        y   <= a_r * b_r + c_r;      // multiply and add in one cycle
        out_valid <= v_r;
    end
endmodule

`default_nettype wire
