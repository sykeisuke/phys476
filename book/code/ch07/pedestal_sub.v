`timescale 1ns / 1ps
`default_nettype none

// Pedestal subtraction for a 12-bit ADC sample.
// The result is signed (noise can go below the pedestal), so it needs
// 13 bits: -4095 ... +4095.
module pedestal_sub (
    input  wire        [11:0] adc,
    input  wire        [11:0] pedestal,
    output wire signed [12:0] amp
);
    assign amp = $signed({1'b0, adc})
               - $signed({1'b0, pedestal});
endmodule

`default_nettype wire
