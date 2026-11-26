`timescale 1ns / 1ps
`default_nettype none

// Hierarchy example from the 2026 lecture: three leaf modules, a sub-module
// that uses all three, and a top that uses the sub-module three times.
module leaf_A (input wire x, output wire y);
    assign y = ~x;
endmodule

module leaf_B (input wire x, output wire y);
    assign y = x & 1'b1;
endmodule

module leaf_C (input wire x, output wire y);
    assign y = x | 1'b0;
endmodule

module sub_module (
    input  wire in,
    output wire out
);
    wire w1, w2, w3;
    leaf_A uA (.x(in), .y(w1));
    leaf_B uB (.x(in), .y(w2));
    leaf_C uC (.x(in), .y(w3));
    assign out = w1 ^ w2 ^ w3;      // = ~in ^ in ^ in = ~in
endmodule

module top (
    input  wire [2:0] in,
    output wire [2:0] out
);
    sub_module u0 (.in(in[0]), .out(out[0]));
    sub_module u1 (.in(in[1]), .out(out[1]));
    sub_module u2 (.in(in[2]), .out(out[2]));
endmodule

`default_nettype wire
