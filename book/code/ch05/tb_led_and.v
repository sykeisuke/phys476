`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench: all 256 switch settings.
module tb_led_and;
    reg  [7:0] sw;
    wire [7:0] led;
    integer i, errors = 0;

    led_and dut (.sw(sw), .led(led));

    initial begin
        for (i = 0; i < 256; i = i + 1) begin
            sw = i;
            #10;
            if (led !== {sw[7:1], sw[0] & sw[1]}) begin
                $display("FAIL: sw = %b  led = %b", sw, led);
                errors = errors + 1;
            end
        end
        if (errors == 0) $display("PASS");
        else             $display("FAILED with %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
