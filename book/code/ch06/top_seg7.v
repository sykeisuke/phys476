`timescale 1ns / 1ps
`default_nettype none

// Board top for the Nexys Video (it has no seven-segment display):
// the hex digit on SW[3:0] is decoded by seg7 and shown on LED[6:0].
// LED0 = segment a, LED1 = b, ..., LED6 = g (seg = gfedcba); LED7 is off.
// SW[7:4] are not used. Use with book/code/ch05/nexys_video.xdc (ports sw[7:0], led[7:0]).
module top_seg7 (
    input  wire [7:0] sw,
    output wire [7:0] led
);
    seg7 u_seg (.hex(sw[3:0]), .seg(led[6:0]));
    assign led[7] = 1'b0;
endmodule

`default_nettype wire
