`timescale 1ns / 1ps
`default_nettype none

// 4-to-1 multiplexer for 8-bit data.
module mux4 (
    input  wire [7:0] d0, d1, d2, d3,
    input  wire [1:0] sel,
    output reg  [7:0] y
);
    always @(*) begin
        case (sel)
            2'd0:    y = d0;
            2'd1:    y = d1;
            2'd2:    y = d2;
            default: y = d3;
        endcase
    end
endmodule

`default_nettype wire
