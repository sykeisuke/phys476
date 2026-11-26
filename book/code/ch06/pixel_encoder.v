`timescale 1ns / 1ps
`default_nettype none

// Hit encoder for an N-pixel sensor. Reports the number of the
// lowest-numbered pixel that is hit, and whether any pixel is hit.
module pixel_encoder #(
    parameter N = 64,
    parameter W = 6            // index width, W = log2(N)
) (
    input  wire [N-1:0] hit,
    output reg  [W-1:0] idx,
    output wire         valid
);
    integer i;
    assign valid = |hit;

    always @(*) begin
        idx = {W{1'b0}};
        // Loop from the top down, so that the lowest hit pixel
        // is the last assignment and wins.
        for (i = N - 1; i >= 0; i = i - 1)
            if (hit[i])
                idx = i[W-1:0];
    end
endmodule

`default_nettype wire
