`timescale 1ns / 1ps
`default_nettype none

// Clock-enable tick: 'tick' is 1 for one clock cycle every DIV cycles.
// Use it as an enable (if (tick) ...), never as a clock.
// 100 MHz clock, DIV = 20_000_000 -> one tick every 200 ms (5 Hz).
module tick_gen #(
    parameter DIV = 20_000_000,
    parameter W = 25              // enough bits for DIV-1 (2^25 > 2e7)
) (
    input  wire clk,
    input  wire rst,
    output reg  tick
);
    reg [W-1:0] cnt;

    always @(posedge clk) begin
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
    end
endmodule

`default_nettype wire
