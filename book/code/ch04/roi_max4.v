`timescale 1ns / 1ps
`default_nettype none

// Index of the largest of four 8-bit values. Ties go to the lower index.
module roi_max4 (
    input  wire [7:0] e0, e1, e2, e3,
    output reg  [1:0] roi
);
    reg [7:0] best;
    always @(*) begin
        roi  = 2'd0;
        best = e0;
        if (e1 > best) begin roi = 2'd1; best = e1; end
        if (e2 > best) begin roi = 2'd2; best = e2; end
        if (e3 > best) begin roi = 2'd3; best = e3; end
    end
endmodule

`default_nettype wire
