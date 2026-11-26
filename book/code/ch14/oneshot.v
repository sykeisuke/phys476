`timescale 1ns / 1ps
`default_nettype none

// One-shot timer (pulse generator): a trigger makes 'out' high for N cycles.
// retrig = 1: a trigger during the pulse restarts it.
// retrig = 0: triggers during the pulse are ignored.
module oneshot #(
    parameter N = 100,
    parameter W = 8
) (
    input  wire clk,
    input  wire rst,
    input  wire trig,
    input  wire retrig,
    output wire out
);
    reg [W-1:0] cnt;              // cycles left; 0 = idle

    assign out = (cnt != 0);

    always @(posedge clk) begin
        if (rst)
            cnt <= {W{1'b0}};
        else if (trig && (!out || retrig))
            cnt <= N;
        else if (out)
            cnt <= cnt - 1'b1;
    end
endmodule

`default_nettype wire
