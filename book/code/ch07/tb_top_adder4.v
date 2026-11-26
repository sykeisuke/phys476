`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for top_adder4: all 256 switch settings.
module tb_top_adder4;
    reg  [7:0] sw;
    wire [7:0] led;
    integer i, errors = 0;

    top_adder4 dut (.sw(sw), .led(led));

    initial begin
        for (i = 0; i < 256; i = i + 1) begin
            sw = i[7:0];
            #1;
            if (led !== {3'b000, ({1'b0, sw[3:0]} + {1'b0, sw[7:4]})}) begin
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
