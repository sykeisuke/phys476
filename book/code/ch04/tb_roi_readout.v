`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for roi_readout.
module tb_roi_readout;
    reg  [7:0] e [0:3];
    reg  [9:0] th;
    wire       trig;
    wire [1:0] roi;
    wire [7:0] data_out;
    integer    errors = 0;
    integer    i, k;

    roi_readout dut (.e0(e[0]), .e1(e[1]), .e2(e[2]), .e3(e[3]), .th(th),
                     .trig(trig), .roi(roi), .data_out(data_out));

    // reference model, written differently from the design
    task check;
        reg [9:0] s;
        reg [1:0] r;
        reg       t;
        reg [7:0] d;
        begin
            #1;
            s = e[0] + e[1] + e[2] + e[3];
            r = 0;
            for (k = 1; k < 4; k = k + 1)
                if (e[k] > e[r]) r = k;
            t = (s > th);
            d = t ? e[r] : 8'd0;
            if (trig !== t || roi !== r || data_out !== d) begin
                $error("e=%0d,%0d,%0d,%0d th=%0d: got %b/%0d/%0d, want %b/%0d/%0d",
                       e[0], e[1], e[2], e[3], th, trig, roi, data_out, t, r, d);
                errors = errors + 1;
            end
        end
    endtask

    initial begin
        th = 10'd200;
        e[0] = 10; e[1] = 20; e[2] = 30; e[3] = 5;   check;  // below threshold
        e[0] = 40; e[1] = 10; e[2] = 180; e[3] = 5;  check;  // ROI = 2
        e[0] = 90; e[1] = 90; e[2] = 50; e[3] = 1;   check;  // tie: ROI = 0
        e[0] = 0; e[1] = 0; e[2] = 0; e[3] = 255;    check;  // ROI = 3
        for (i = 0; i < 1000; i = i + 1) begin
            e[0] = $random; e[1] = $random; e[2] = $random; e[3] = $random;
            th = $random;
            check;
        end
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
