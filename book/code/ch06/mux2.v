`timescale 1ns / 1ps
`default_nettype none

// 2-to-1 multiplexer, written twice: from gates, and with ?:
module mux2 (
    input  wire d0, d1, s,
    output wire y_gates,
    output wire y
);
    assign y_gates = (d0 & ~s) | (d1 & s);
    assign y       = s ? d1 : d0;
endmodule

`default_nettype wire
