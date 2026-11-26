`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for the Chapter 11 blocks.
module tb_ch11;
    integer errors = 0;
    integer i, ticks;

    task check(input cond, input [8*24-1:0] what);
        if (!cond) begin
            $error("%0s", what);
            errors = errors + 1;
        end
    endtask

    reg clk = 1'b0;
    always #5 clk = ~clk;              // 100 MHz

    reg rst = 1'b1, ce = 1'b0, sin = 1'b0;

    wire [3:0] sq;
    shift_reg4 u_sr (.clk(clk), .rst(rst), .ce(ce), .sin(sin), .q(sq));

    wire [3:0] cq;
    wire       ctc, cceo;
    counter4 u_c4 (.clk(clk), .rst(rst), .ce(ce), .q(cq), .tc(ctc), .ceo(cceo));

    wire [7:0] q8;
    wire       ceo8;
    counter8_cascade u_c8 (.clk(clk), .rst(rst), .ce(ce), .q(q8), .ceo(ceo8));

    wire tick;
    tick_gen #(.DIV(5), .W(3)) u_tick (.clk(clk), .rst(rst), .tick(tick));

    reg  arst_n = 1'b1;
    wire srst;
    reset_sync u_rs (.clk(clk), .arst_n(arst_n), .rst(srst));

    wire [3:0] led;
    sipo_board #(.DIV(4)) u_board (.clk(clk), .cpu_resetn(arst_n), .sw(sin), .led(led));

    // drive inputs just after the rising edge, check before the next one
    task step;
        begin @(posedge clk); #1; end
    endtask

    reg [3:0] pattern;
    initial begin
        // ------------------------------------------- reset everything
        step; step;
        check(sq === 4'd0 && cq === 4'd0 && q8 === 8'd0, "reset");
        rst = 0;

        // ------------------------------------------- shift register 1011
        pattern = 4'b1011;
        ce = 1;
        for (i = 3; i >= 0; i = i - 1) begin
            sin = pattern[i];
            step;
        end
        check(sq === 4'b1011, "shift 1011");
        ce = 0; sin = 0; step;
        check(sq === 4'b1011, "shift holds when ce = 0");

        // ------------------------------------------- counter starts at 0000
        rst = 1; step; rst = 0;
        check(cq === 4'b0000 && q8 === 8'd0, "count starts at 0");
        ce = 1;
        for (i = 1; i <= 15; i = i + 1) begin
            check(cceo === (i == 16), "no ceo before 1111");
            step;
            check(cq === i[3:0], "count up");
        end
        check(ctc === 1'b1 && cceo === 1'b1, "tc and ceo at 1111");
        ce = 0; #1;
        check(ctc === 1'b1 && cceo === 1'b0, "ceo needs ce");
        ce = 1; step;
        check(cq === 4'b0000, "wraps to 0000");

        // ------------------------------------------- cascade = 8-bit counter
        rst = 1; step; rst = 0;
        for (i = 1; i <= 300; i = i + 1) begin
            step;
            check(q8 === i[7:0], "cascade count");
        end
        ce = 0;

        // ------------------------------------------- tick every 5 clocks
        rst = 1; step; rst = 0;
        ticks = 0;
        for (i = 0; i < 50; i = i + 1) begin
            step;
            ticks = ticks + tick;
        end
        check(ticks == 10, "tick every DIV clocks");

        // ------------------------------------------- reset synchronizer
        #2 arst_n = 0; #1;                 // assert between edges
        check(srst === 1'b1, "rst asserts at once");
        #1 arst_n = 1;
        step; check(srst === 1'b1, "rst held 1st edge");
        step; check(srst === 1'b0, "rst released 2nd edge");

        // ------------------------------------------- board top: shift on ticks
        sin = 1;
        for (i = 0; i < 4 * 4 + 4; i = i + 1) step;
        check(led === 4'b1111, "board shifts on ticks");

        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
