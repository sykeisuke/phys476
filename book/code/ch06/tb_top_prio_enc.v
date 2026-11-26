`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for top_prio_enc: all 256 switch settings.
module tb_top_prio_enc;
    reg  [7:0] sw;
    wire [7:0] led;
    integer i, k, expect_idx, errors = 0;

    top_prio_enc dut (.sw(sw), .led(led));

    initial begin
        for (i = 0; i < 256; i = i + 1) begin
            sw = i[7:0];
            #1;
            expect_idx = 0;
            for (k = 0; k < 8; k = k + 1)
                if (sw[k]) expect_idx = k;          // highest set bit wins
            if (led[2:0] !== expect_idx[2:0] || led[7] !== (sw != 8'd0) ||
                led[6:3] !== 4'b0000) begin
                $error("sw=%b led=%b", sw, led);
                errors = errors + 1;
            end
        end
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
