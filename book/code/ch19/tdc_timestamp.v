`timescale 1ns / 1ps
`default_nettype none

// Multi-hit TDC front end: a free-running counter gives every hit a time stamp.
// Each rising edge of hit_in (asynchronous, at least one clock period wide)
// produces one output word {channel, timestamp} with ts_valid high for one
// cycle; connect it to a FIFO. Time differences are computed later
// (in the FPGA or on the PC), so hits are never lost while a "measurement" runs.
module tdc_timestamp #(
    parameter W  = 32,            // timestamp width: 2^32 x 10 ns = 43 s
    parameter CH = 0              // channel number put into the word
) (
    input  wire         clk,
    input  wire         rst,      // synchronous, active high
    input  wire         hit_in,
    output reg  [W+3:0] ts_data,  // {4-bit channel, timestamp}
    output reg          ts_valid
);
    reg [2:0]   sync;
    reg [W-1:0] now;
    wire        rise = sync[1] & ~sync[2];

    always @(posedge clk) begin
        sync     <= {sync[1:0], hit_in};
        ts_valid <= 1'b0;
        if (rst) begin
            now     <= {W{1'b0}};
            ts_data <= {(W+4){1'b0}};
        end else begin
            now <= now + 1'b1;
            if (rise) begin
                ts_data  <= {CH[3:0], now};
                ts_valid <= 1'b1;
            end
        end
    end
endmodule

`default_nettype wire
