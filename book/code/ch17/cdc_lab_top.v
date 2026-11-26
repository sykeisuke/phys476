`timescale 1ns / 1ps
`default_nettype none

// Lab design for report_cdc: two unrelated clocks, three good crossings
// and one deliberately bad one (a 4-bit counter sampled bit by bit).
// Synthesize it with cdc_lab.tcl and read the report; it is not meant
// for the board.
module cdc_lab_top (
    input  wire       clk_a,
    input  wire       clk_b,
    input  wire       arst_n,
    input  wire       level_a,     // slow level in domain A
    input  wire       pulse_a,     // one-cycle pulse in domain A
    output wire       level_b,
    output wire       pulse_b,
    output reg  [3:0] count_b      // BAD: bus crossed without a synchronizer
);
    wire rst_a, rst_b;
    reset_sync u_rsa (.clk(clk_a), .arst_n(arst_n), .rst(rst_a));
    reset_sync u_rsb (.clk(clk_b), .arst_n(arst_n), .rst(rst_b));

    // good: register in A, then 2-FF synchronizer in B
    reg level_r = 1'b0;
    always @(posedge clk_a) level_r <= level_a;
    sync_2ff u_lvl (.clk(clk_b), .d(level_r), .q(level_b));

    // good: toggle pulse synchronizer
    pulse_sync u_pls (.clk_a(clk_a), .pulse_a(pulse_a), .clk_b(clk_b), .pulse_b(pulse_b));

    // bad: binary counter in A read directly in B
    reg [3:0] count_a = 4'd0;
    always @(posedge clk_a)
        if (rst_a) count_a <= 4'd0;
        else       count_a <= count_a + 4'd1;

    always @(posedge clk_b)
        if (rst_b) count_b <= 4'd0;
        else       count_b <= count_a;
endmodule

`default_nettype wire
