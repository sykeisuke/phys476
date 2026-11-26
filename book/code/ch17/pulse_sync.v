`timescale 1ns / 1ps
`default_nettype none

// Pulse synchronizer: a one-cycle pulse in clk_a becomes a one-cycle
// pulse in clk_b. Pulses must be at least about 3 clk_b cycles apart.
module pulse_sync (
    input  wire clk_a,
    input  wire pulse_a,
    input  wire clk_b,
    output wire pulse_b
);
    reg tog_a = 1'b0;                 // flips once per input pulse
    always @(posedge clk_a)
        if (pulse_a)
            tog_a <= ~tog_a;

    wire tog_b;
    sync_2ff u_sync (.clk(clk_b), .d(tog_a), .q(tog_b));

    reg tog_b_d = 1'b0;               // previous value, for edge detection
    always @(posedge clk_b)
        tog_b_d <= tog_b;

    assign pulse_b = tog_b ^ tog_b_d; // any change = one pulse
endmodule

`default_nettype wire
