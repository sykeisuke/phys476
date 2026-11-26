`timescale 1ns / 1ps
`default_nettype none

// First simulation (from Lecture 9/11 of 2026): three gates on two switches.
module example_sim (
    input  wire SW0,
    input  wire SW1,
    output wire LED0,   // AND
    output wire LED1,   // OR
    output wire LED2    // XOR
);
    assign LED0 = SW0 & SW1;
    assign LED1 = SW0 | SW1;
    assign LED2 = SW0 ^ SW1;
endmodule

`default_nettype wire
