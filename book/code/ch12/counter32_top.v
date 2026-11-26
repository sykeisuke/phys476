`timescale 1ns / 1ps
`default_nettype none

// 32-bit counter on the 100 MHz clock of the Nexys Video.
// LED k shows bit 24 + k, which toggles every 2^(24+k) clock periods.
// The reset button goes through reset_sync (course rule for board resets).
module counter32_top (
    input  wire       clk,          // 100 MHz, pin R4
    input  wire       cpu_resetn,   // reset button, active low
    output wire [7:0] led
);
    wire rst;
    reset_sync u_rst (.clk(clk), .arst_n(cpu_resetn), .rst(rst));

    reg [31:0] cnt = 32'd0;
    always @(posedge clk)
        if (rst)
            cnt <= 32'd0;
        else
            cnt <= cnt + 32'd1;

    assign led = cnt[31:24];
endmodule

`default_nettype wire
