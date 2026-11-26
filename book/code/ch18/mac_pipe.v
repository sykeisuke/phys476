`timescale 1ns / 1ps
`default_nettype none

// y = a*b + c, pipelined: a register between the multiplier
// and the adder. Latency 3 cycles, still one result per cycle
// (II = 1), and each stage holds only one block of logic.
module mac_pipe (
    input  wire        clk,
    input  wire        in_valid,
    input  wire [7:0]  a, b,
    input  wire [15:0] c,
    output reg         out_valid,
    output reg  [16:0] y
);
    reg [7:0]  a_r, b_r;
    reg [15:0] c_r, c_d;
    reg [15:0] p;
    reg        v_r, v_p;

    initial begin
        v_r = 1'b0;
        v_p = 1'b0;
        out_valid = 1'b0;
    end

    always @(posedge clk) begin
        // stage 0: input registers
        a_r <= a;
        b_r <= b;
        c_r <= c;
        v_r <= in_valid;
        // stage 1: multiply; delay c to stay aligned
        p   <= a_r * b_r;
        c_d <= c_r;
        v_p <= v_r;
        // stage 2: add
        y   <= p + c_d;
        out_valid <= v_p;
    end
endmodule

`default_nettype wire
