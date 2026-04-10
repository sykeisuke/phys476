module top (
    input  wire        clk,
    input  wire        rst,
    input  wire        ap_start,
    output wire        ap_done,
    output wire        ap_idle,
    output wire        ap_ready,

    output wire [3:0]  a_address0,
    output wire        a_ce0,
    input  wire [31:0] a_q0,

    output wire [3:0]  b_address0,
    output wire        b_ce0,
    input  wire [31:0] b_q0,

    output wire [31:0] result
);

    muladd_0 u_muladd (
        .ap_clk(clk),
        .ap_rst(rst),
        .ap_start(ap_start),
        .ap_done(ap_done),
        .ap_idle(ap_idle),
        .ap_ready(ap_ready),
        .a_address0(a_address0),
        .a_ce0(a_ce0),
        .a_q0(a_q0),
        .b_address0(b_address0),
        .b_ce0(b_ce0),
        .b_q0(b_q0),
        .ap_return(result)
    );

endmodule
