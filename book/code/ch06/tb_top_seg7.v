`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for top_seg7: all 16 digits, and SW[7:4] must not matter.
// Run: iverilog -g2005 -Wall -o sim tb_top_seg7.v top_seg7.v seg7.v && vvp sim
module tb_top_seg7;
    reg  [7:0] sw;
    wire [7:0] led;
    reg  [6:0] want [0:15];      // expected LED[6:0] = gfedcba, written from the digit shapes
    integer i, errors = 0;

    top_seg7 dut (.sw(sw), .led(led));

    initial begin
        want[0]  = 7'b0111111;  want[1]  = 7'b0000110;  want[2]  = 7'b1011011;
        want[3]  = 7'b1001111;  want[4]  = 7'b1100110;  want[5]  = 7'b1101101;
        want[6]  = 7'b1111101;  want[7]  = 7'b0000111;  want[8]  = 7'b1111111;
        want[9]  = 7'b1101111;  want[10] = 7'b1110111;  want[11] = 7'b1111100;
        want[12] = 7'b0111001;  want[13] = 7'b1011110;  want[14] = 7'b1111001;
        want[15] = 7'b1110001;
        for (i = 0; i < 256; i = i + 1) begin
            sw = i[7:0];
            #1;
            if (led !== {1'b0, want[i % 16]}) begin
                $error("sw=%b led=%b want=%b", sw, led, {1'b0, want[i % 16]});
                errors = errors + 1;
            end
        end
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
