`timescale 1ns / 1ps
`default_nettype none

// Two-flip-flop synchronizer for one asynchronous bit (button, switch,
// signal from another clock). Nothing may sit between the two flip-flops.
module sync_2ff (
    input  wire clk,
    input  wire d_async,
    output wire q
);
    (* ASYNC_REG = "TRUE" *)
    reg s1 = 1'b0;
    (* ASYNC_REG = "TRUE" *)
    reg s2 = 1'b0;

    always @(posedge clk) begin
        s1 <= d_async;            // may go metastable
        s2 <= s1;                 // has one clock period to settle
    end

    assign q = s2;
endmodule

`default_nettype wire
