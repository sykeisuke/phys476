`timescale 1ns / 1ps
`default_nettype none

// Sum four 10-bit energy deposits and trigger when the sum exceeds
// a programmable threshold. The sum needs 12 bits: 4 * 1023 = 4092.
module energy_trigger (
    input  wire [9:0]  e0, e1, e2, e3,
    input  wire [11:0] threshold,
    output wire [11:0] esum,
    output wire        trig
);
    assign esum = e0 + e1 + e2 + e3;
    assign trig = (esum > threshold);
endmodule

`default_nettype wire
