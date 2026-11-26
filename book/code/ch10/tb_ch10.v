`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for the Chapter 10 storage elements.
module tb_ch10;
    integer errors = 0;
    integer i, flips;

    task check(input cond, input [8*24-1:0] what);
        if (!cond) begin
            $error("%0s", what);
            errors = errors + 1;
        end
    endtask

    // ------------------------------------------------ clock: 10 ns period
    reg clk = 1'b0;
    always #5 clk = ~clk;

    // ------------------------------------------------ devices under test
    reg  s = 0, r = 0;
    wire q_sr, qn_sr;
    sr_latch u_sr (.s(s), .r(r), .q(q_sr), .qn(qn_sr));

    reg  en = 0, d = 0;
    wire q_lat, q_ff;
    d_latch u_lat (.en(en), .d(d), .q(q_lat));
    dff     u_ff  (.clk(clk), .d(d), .q(q_ff));

    reg  rst = 0, clr = 0, ce = 0;
    wire q_fdre, q_fdce;
    dff_ce_srst u_fdre (.clk(clk), .rst(rst), .ce(ce), .d(d), .q(q_fdre));
    dff_ce_arst u_fdce (.clk(clk), .clr(clr), .ce(ce), .d(d), .q(q_fdce));

    reg  t = 0;
    wire q_t;
    tff u_t (.clk(clk), .rst(rst), .t(t), .q(q_t));

    reg  set = 0;
    wire q_rse;
    dff_rse u_rse (.clk(clk), .rst(rst), .set(set), .ce(ce), .d(d), .q(q_rse));

    wire [2:0] q_rip;
    ripple3 u_rip (.clk(clk), .clr(clr), .q(q_rip));

    always @(q_sr) flips = flips + 1;

    initial begin
        // ---------------- SR latch: reset, hold, set, hold
        r = 1; #5;  check(q_sr === 0 && qn_sr === 1, "sr reset");
        r = 0; #5;  check(q_sr === 0 && qn_sr === 1, "sr hold 0");
        s = 1; #5;  check(q_sr === 1 && qn_sr === 0, "sr set");
        s = 0; #5;  check(q_sr === 1 && qn_sr === 0, "sr hold 1");
        // (1,1): both outputs 0, so q and qn are no longer complements
        s = 1; r = 1; #5;
        check(q_sr === 0 && qn_sr === 0, "sr (1,1)");
        // (1,1) -> (0,0) at the same instant: a race. With equal gate
        // delays the model oscillates; real gates settle to 0 or 1.
        flips = 0;
        s = 0; r = 0; #20;
        $display("SR race: q changed %0d times in 20 ns", flips);
        check(flips > 2, "sr race oscillates");
        r = 1; #5; r = 0; #5;     // a clean reset ends the race
        check(q_sr === 0 && qn_sr === 1, "sr recover");

        // ---------------- D latch vs D flip-flop (clk rises at 5, 15, 25 ...)
        @(negedge clk);           // t = 10k ns, clk falls
        en = 1; d = 1; #1;
        check(q_lat === 1, "latch transparent");
        check(q_ff  !== 1, "ff waits for edge");
        @(posedge clk); #1;
        check(q_ff === 1, "ff captures at edge");
        en = 0; d = 0; #1;
        check(q_lat === 1, "latch holds");
        check(q_ff  === 1, "ff holds between edges");
        @(posedge clk); #1;
        check(q_ff === 0, "ff captures 0");

        // ---------------- FDRE-like and FDCE-like flip-flops
        rst = 1; clr = 1; #1;
        check(q_fdce === 0, "async clear acts at once");
        @(posedge clk); #1;
        check(q_fdre === 0, "sync reset at edge");
        rst = 0; clr = 0; d = 1; ce = 0;
        @(posedge clk); #1;
        check(q_fdre === 0 && q_fdce === 0, "ce = 0 holds");
        ce = 1;
        @(posedge clk); #1;
        check(q_fdre === 1 && q_fdce === 1, "ce = 1 loads");
        rst = 1; ce = 1;
        @(posedge clk); #1;
        check(q_fdre === 0, "reset wins over ce");
        rst = 0; ce = 0;

        // ---------------- reset > set > clock enable
        rst = 1; set = 1; ce = 1; d = 1;
        @(posedge clk); #1;
        check(q_rse === 1'b0, "rse: reset wins");
        rst = 0;
        @(posedge clk); #1;
        check(q_rse === 1'b1, "rse: set wins over ce");
        set = 0; d = 0;
        @(posedge clk); #1;
        check(q_rse === 1'b0, "rse: ce loads d");
        ce = 0; d = 1;
        @(posedge clk); #1;
        check(q_rse === 1'b0, "rse: ce = 0 holds");
        ce = 0; d = 0;

        // ---------------- T flip-flop: toggles every edge while t = 1
        t = 1;
        for (i = 1; i <= 4; i = i + 1) begin
            @(posedge clk); #1;
            check(q_t === i[0], "tff toggles");
        end
        t = 0;
        @(posedge clk); #1;
        check(q_t === 0, "tff holds");

        // ---------------- ripple counter counts falling clock edges
        clr = 1; #1; clr = 0;
        check(q_rip === 3'd0, "ripple clear");
        for (i = 1; i <= 10; i = i + 1) begin
            @(negedge clk); #2;   // wait for the ripple to settle
            check(q_rip === i[2:0], "ripple count");
        end

        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
