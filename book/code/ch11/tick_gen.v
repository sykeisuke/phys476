`timescale 1ns / 1ps
`default_nettype none

// One-clock-wide tick every DIV clock cycles. Use it as a clock enable.
// DIV = 100_000_000 gives one tick per second from a 100 MHz clock.
module tick_gen #(
    parameter DIV = 100_000_000,
    parameter W   = 27              // enough bits for DIV - 1
) (
    input  wire clk,
    input  wire rst,
    output reg  tick
);
    reg [W-1:0] cnt;
    always @(posedge clk)
        if (rst) begin
            cnt  <= {W{1'b0}};
            tick <= 1'b0;
        end else if (cnt == DIV - 1) begin
            cnt  <= {W{1'b0}};
            tick <= 1'b1;
        end else begin
            cnt  <= cnt + 1'b1;
            tick <= 1'b0;
        end
endmodule

`default_nettype wire
