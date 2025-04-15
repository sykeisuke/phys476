`timescale 1ns/1ps

module top (
    input  wire        clk,         // 100 MHz clock
    input  wire        rst,         // active-high reset
    input  wire        start,       // start signal
    input  wire [47:0] fc1_input,   // input vector
    input  wire        fc1_input_vld, // input valid

    output wire [15:0] layer13_out,  // output result
    output wire        layer13_out_vld, // output valid
    output wire        done         // done flag
);

  // internal signals
  wire ap_ready;
  wire ap_idle;

  myproject_0 hls_inst (
    .ap_clk(clk),
    .ap_rst(rst),
    .ap_start(start),
    .ap_done(done),
    .ap_idle(ap_idle),
    .ap_ready(ap_ready),

    .fc1_input(fc1_input),
    .fc1_input_ap_vld(fc1_input_vld),

    .layer13_out(layer13_out),
    .layer13_out_ap_vld(layer13_out_vld)
  );

endmodule
