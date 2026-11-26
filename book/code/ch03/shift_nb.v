`timescale 1ns / 1ps
`default_nettype none

// Three-stage shift register: correct, uses non-blocking (<=).
module shift_nb (
    input  wire clk,
    input  wire din,
    output reg  q1, q2, q3
);
    always @(posedge clk) begin
        q1 <= din;
        q2 <= q1;
        q3 <= q2;
    end
endmodule

`default_nettype wire
