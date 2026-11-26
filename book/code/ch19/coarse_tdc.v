`timescale 1ns / 1ps
`default_nettype none

// Coarse TDC: counts clock periods between a start and a stop pulse.
// start_in and stop_in are asynchronous detector pulses (at least one clock
// period wide). Both pass through the same 2-FF synchronizer, so the
// synchronizer latency cancels in the difference.
// At 100 MHz one count is 10 ns. A start is ignored while a measurement runs.
module coarse_tdc #(
    parameter W    = 16,
    parameter TMAX = 2000            // give up after TMAX counts (20 us)
) (
    input  wire         clk,
    input  wire         rst,         // synchronous, active high
    input  wire         start_in,
    input  wire         stop_in,
    output reg  [W-1:0] tdc_count,   // stop - start in clock periods
    output reg          valid,       // one-cycle pulse: tdc_count is new
    output reg          timeout      // one-cycle pulse: no stop within TMAX
);
    reg [2:0]   start_sync, stop_sync;
    reg [W-1:0] cnt;
    reg         running;

    wire start_rise = start_sync[1] & ~start_sync[2];
    wire stop_rise  = stop_sync[1]  & ~stop_sync[2];

    always @(posedge clk) begin
        start_sync <= {start_sync[1:0], start_in};
        stop_sync  <= {stop_sync[1:0],  stop_in};
        valid   <= 1'b0;
        timeout <= 1'b0;
        if (rst) begin
            running   <= 1'b0;
            cnt       <= {W{1'b0}};
            tdc_count <= {W{1'b0}};
        end else if (!running) begin
            if (start_rise) begin
                running <= 1'b1;
                cnt     <= {W{1'b0}};
            end
        end else if (stop_rise) begin
            tdc_count <= cnt + 1'b1;
            valid     <= 1'b1;
            running   <= 1'b0;
        end else if (cnt == TMAX - 1) begin
            timeout <= 1'b1;
            running <= 1'b0;
        end else begin
            cnt <= cnt + 1'b1;
        end
    end
endmodule

`default_nettype wire
