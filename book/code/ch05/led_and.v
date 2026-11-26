`timescale 1ns / 1ps
`default_nettype none

// First board design for the Nexys Video: LED0 = SW0 & SW1.
// The other LEDs show their own switches, a quick check of the wiring.
module led_and (
    input  wire [7:0] sw,
    output wire [7:0] led
);
    assign led[0]   = sw[0] & sw[1];
    assign led[7:1] = sw[7:1];
endmodule

`default_nettype wire
