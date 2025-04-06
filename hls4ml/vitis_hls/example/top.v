module top (
    input wire clk,
    input wire rst,
    input wire [15:0] a [0:15],
    input wire [15:0] b [0:15],
    output wire [31:0] result
);

    wire ap_start = 1'b1;
    wire ap_done;
    wire ap_idle;
    wire ap_ready;

    muladd u_muladd (
        .ap_clk(clk),
        .ap_rst(rst),
        .ap_start(ap_start),
        .ap_done(ap_done),
        .ap_idle(ap_idle),
        .ap_ready(ap_ready),
        .a_address(), .a_ce0(), .a_q0(), 
        .b_address(), .b_ce0(), .b_q0(), 
        .ap_return(result)
    );

endmodule

