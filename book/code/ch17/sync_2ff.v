`timescale 1ns / 1ps
`default_nettype none

// Two-flip-flop synchronizer for a slowly changing level signal.
// 'd' comes from another clock domain; 'q' is safe to use in clk's domain
// two to three clk cycles later. Never use it for a bus.
module sync_2ff (
    input  wire clk,
    input  wire d,
    output wire q
);
    (* ASYNC_REG = "TRUE" *) reg s1 = 1'b0;
    (* ASYNC_REG = "TRUE" *) reg s2 = 1'b0;

    always @(posedge clk) begin
        s1 <= d;
        s2 <= s1;
    end

    assign q = s2;
endmodule

`default_nettype wire
