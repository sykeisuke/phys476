`timescale 1ns / 1ps
`default_nettype none

// A clock from the Clocking Wizard (IP name clk_wiz_0, one output),
// with the logic held in reset until the MMCM reports locked.
module mmcm_top (
    input  wire       clk,          // 100 MHz board clock
    input  wire       cpu_resetn,
    output wire [7:0] led
);
    wire clk_sys, locked;

    clk_wiz_0 u_clk (
        .clk_in1  (clk),
        .reset    (~cpu_resetn),    // MMCM reset is active high
        .clk_out1 (clk_sys),
        .locked   (locked)
    );

    // hold the logic in reset until the clock is stable
    wire rst;
    reset_sync u_rst (.clk(clk_sys), .arst_n(locked), .rst(rst));

    reg [31:0] cnt;
    always @(posedge clk_sys)
        if (rst)
            cnt <= 32'd0;
        else
            cnt <= cnt + 32'd1;

    assign led = cnt[31:24];
endmodule

`default_nettype wire
