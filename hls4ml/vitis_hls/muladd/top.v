module top (
    input wire clk,
    input wire rst,
    input wire [31:0] a [0:15],
    input wire [31:0] b [0:15],
    output wire [31:0] result
);

    wire ap_start = 1'b1;
    wire ap_done;
    wire ap_idle;
    wire ap_ready;

    wire [3:0] a_address0;
    wire a_ce0;
    reg  [31:0] a_q0;

    wire [3:0] b_address0;
    wire b_ce0;
    reg  [31:0] b_q0;

    always @(*) begin
        a_q0 = a[a_address0];
        b_q0 = b[b_address0];
    end

    muladd u_muladd (
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
