`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for top_sub3 and top_sub3_signmag: all 256 switch settings.
// Run: iverilog -g2005 -Wall -o sim tb_top_sub3.v top_sub3.v sub3_signmag.v && vvp sim
module tb_top_sub3;
    reg  [7:0] sw;
    wire [7:0] led_tc, led_sm;
    integer i, a, b, d, errors = 0;
    reg  [3:0] want_tc, want_sm;

    top_sub3         u_tc (.sw(sw), .led(led_tc));
    top_sub3_signmag u_sm (.sw(sw), .led(led_sm));

    initial begin
        for (i = 0; i < 256; i = i + 1) begin
            sw = i[7:0];
            a  = i % 8;                    // SW[2:0]
            b  = (i / 8) % 8;              // SW[5:3]
            d  = a - b;                    // -7 .. +7
            want_tc = d;                   // low 4 bits = two's complement
            want_sm = (d < 0) ? {1'b1, 3'b000} | (-d) : d;
            #1;
            if (led_tc !== {4'b0000, want_tc} || led_sm !== {4'b0000, want_sm}) begin
                $error("a=%0d b=%0d: tc led=%b want %b, sm led=%b want %b",
                       a, b, led_tc[3:0], want_tc, led_sm[3:0], want_sm);
                errors = errors + 1;
            end
        end
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
