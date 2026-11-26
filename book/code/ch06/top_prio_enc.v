`timescale 1ns / 1ps
`default_nettype none

// Board top for the Nexys Video: switches in, encoder result on the LEDs.
// Use with book/code/ch05/nexys_video.xdc (ports sw[7:0], led[7:0]).
module top_prio_enc (
    input  wire [7:0] sw,
    output wire [7:0] led
);
    prio_enc8 u_enc (.in(sw), .idx(led[2:0]), .valid(led[7]));
    assign led[6:3] = 4'b0000;
endmodule

`default_nettype wire
