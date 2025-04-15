`timescale 1ns/1ps

module top (
    input  wire        clk,
    input  wire        rst,
    input  wire        start,
    input  wire [47:0] fc1_input,
    input  wire        fc1_input_vld,

    output wire [15:0] layer13_out,
    output wire        layer13_out_vld,
    output wire        done
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
