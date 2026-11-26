`timescale 1ns / 1ps
`default_nettype none

// Debouncer for a synchronized input. 'in' is sampled on each 'tick'
// (e.g. every 1 ms); 'out' follows only after K equal samples in a row.
module debounce #(
    parameter K = 8,
    parameter W = 4
) (
    input  wire clk,
    input  wire rst,
    input  wire tick,
    input  wire in,               // already synchronized (sync_2ff)
    output reg  out
);
    reg [W-1:0] cnt;

    always @(posedge clk) begin
        if (rst) begin
            out <= 1'b0;
            cnt <= {W{1'b0}};
        end else if (tick) begin
            if (in == out)
                cnt <= {W{1'b0}};             // no change requested
            else if (cnt == K - 1) begin
                out <= in;                    // stable for K samples
                cnt <= {W{1'b0}};
            end else
                cnt <= cnt + 1'b1;
        end
    end
endmodule

`default_nettype wire
