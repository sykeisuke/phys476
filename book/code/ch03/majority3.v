`timescale 1ns / 1ps
`default_nettype none

// Two-out-of-three coincidence: high when at least two of the
// three scintillator hits are high.
module majority3 (
    input  wire [2:0] hit,
    output wire       trig
);
    assign trig = (hit[0] & hit[1]) | (hit[1] & hit[2])
                | (hit[0] & hit[2]);
endmodule

`default_nettype wire
