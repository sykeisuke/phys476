`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for the Chapter 3 examples.
module tb_ch03;
    integer i, errors;

    // ---- majority3: exhaustive test ----
    reg  [2:0] hit;
    wire       trig;
    majority3 u_maj (.hit(hit), .trig(trig));

    // ---- mux4 ----
    reg  [7:0] d0, d1, d2, d3;
    reg  [1:0] sel;
    wire [7:0] y;
    mux4 u_mux (.d0(d0), .d1(d1), .d2(d2), .d3(d3), .sel(sel), .y(y));

    // ---- shift registers ----
    reg  clk = 1'b0, din = 1'b0;
    wire a1, a2, a3, b1, b2, b3;
    shift_nb u_nb (.clk(clk), .din(din), .q1(a1), .q2(a2), .q3(a3));
    shift_b  u_b  (.clk(clk), .din(din), .q1(b1), .q2(b2), .q3(b3));
    always #5 clk = ~clk;

    initial begin
        errors = 0;

        for (i = 0; i < 8; i = i + 1) begin
            hit = i[2:0];
            #1;
            if (trig !== (hit[0] + hit[1] + hit[2] >= 2)) begin
                $error("majority3: hit=%b trig=%b", hit, trig);
                errors = errors + 1;
            end
        end

        d0 = 8'h11; d1 = 8'h22; d2 = 8'h33; d3 = 8'h44;
        for (i = 0; i < 4; i = i + 1) begin
            sel = i[1:0];
            #1;
            if (y !== (8'h11 * (i + 1))) begin
                $error("mux4: sel=%0d y=%h", sel, y);
                errors = errors + 1;
            end
        end

        // Send a single 1 into both shift registers.
        @(negedge clk) din = 1'b1;
        @(negedge clk) din = 1'b0;
        $display("after 1 clock : nb=%b%b%b  b=%b%b%b", a1, a2, a3, b1, b2, b3);
        @(negedge clk);
        $display("after 2 clocks: nb=%b%b%b  b=%b%b%b", a1, a2, a3, b1, b2, b3);
        @(negedge clk);
        $display("after 3 clocks: nb=%b%b%b  b=%b%b%b", a1, a2, a3, b1, b2, b3);

        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
