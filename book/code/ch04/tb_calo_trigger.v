`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for calo_trigger.
module tb_calo_trigger;
    reg  [7:0] e0, e1, e2, e3;
    reg  [9:0] th;
    wire [9:0] esum;
    wire       trig;
    integer    errors = 0;
    integer    i;

    calo_trigger dut (.e0(e0), .e1(e1), .e2(e2), .e3(e3),
                      .th(th), .esum(esum), .trig(trig));

    // apply one input vector, compute the expected result, compare
    task check(input [7:0] a, b, c, d, input [9:0] t);
        reg [9:0] exp_sum;
        reg       exp_trig;
        begin
            e0 = a; e1 = b; e2 = c; e3 = d; th = t;
            #1;
            exp_sum  = a + b + c + d;
            exp_trig = (exp_sum > t);
            if (esum !== exp_sum || trig !== exp_trig) begin
                $error("e=%0d,%0d,%0d,%0d th=%0d: got %0d/%b, want %0d/%b",
                       a, b, c, d, t, esum, trig, exp_sum, exp_trig);
                errors = errors + 1;
            end
        end
    endtask

    initial begin
        // corner cases
        check(0, 0, 0, 0, 0);            // nothing
        check(255, 255, 255, 255, 1000); // largest sum, 1020
        check(100, 100, 100, 100, 400);  // equal to threshold: no trigger
        check(100, 100, 100, 101, 400);  // one count above
        // random events
        for (i = 0; i < 1000; i = i + 1)
            check($random, $random, $random, $random, $random);

        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
