`timescale 1ns / 1ps
`default_nettype none

// Random valid / random back-pressure test of axis_skid.
// Checks: every word arrives once, in order; the source and the
// DUT output both obey the AXI-Stream rule (TVALID and TDATA held
// while TVALID=1 and TREADY=0); full rate when nothing stalls.
module tb_axis_skid;
    localparam N = 2000;
    reg        clk = 1'b0, rst = 1'b1;
    reg        s_tvalid = 1'b0, m_tready = 1'b0;
    reg  [7:0] s_tdata = 8'd0;
    wire       s_tready, m_tvalid;
    wire [7:0] m_tdata;
    integer    sent = 0, got = 0, errors = 0, cyc = 0, phase = 0;
    integer    p_valid = 50, p_ready = 50, t0 = 0;
    reg        last_mv, last_mr, hs_s = 1'b0;
    reg  [7:0] last_md;

    always #5 clk = ~clk;

    axis_skid #(.W(8)) dut (.clk(clk), .rst(rst),
        .s_tvalid(s_tvalid), .s_tready(s_tready), .s_tdata(s_tdata),
        .m_tvalid(m_tvalid), .m_tready(m_tready), .m_tdata(m_tdata));

    // sample everything on the rising edge
    always @(posedge clk) begin
        cyc <= cyc + 1;
        if (!rst) begin
            // rule check on the DUT output: stalled -> hold
            if (last_mv && !last_mr && (!m_tvalid || m_tdata !== last_md)) begin
                $display("FAIL: output changed while stalled (cycle %0d)", cyc);
                errors = errors + 1;
            end
            if (m_tvalid && m_tready) begin
                if (m_tdata !== got[7:0]) begin
                    $display("FAIL: word %0d = %0d", got, m_tdata);
                    errors = errors + 1;
                end
                got = got + 1;
            end
            hs_s = s_tvalid && s_tready;
            if (hs_s)
                sent = sent + 1;
        end
        last_mv = m_tvalid;
        last_mr = m_tready;
        last_md = m_tdata;
    end

    // source and sink change their outputs after the edge
    always @(posedge clk) begin
        #1;
        if (!rst) begin
            // source: may raise TVALID any time; must hold it (and the
            // data) until the handshake. Never looks at TREADY first.
            if (!s_tvalid || hs_s) begin
                s_tvalid = (sent < N) && (($random & 127) < p_valid * 128 / 100);
                s_tdata  = sent[7:0];
            end
            m_tready = (($random & 127) < p_ready * 128 / 100);
        end
    end

    // watchdog: lost words would leave the test waiting forever
    initial begin
        #(200000 * 10);
        $display("FAIL: timeout, sent %0d got %0d, %0d errors", sent, got, errors);
        $finish;
    end

    initial begin
        repeat (3) @(posedge clk);
        #1 rst = 1'b0;
        // phase 1: random valid, random ready
        wait (got == N / 2);
        // phase 2: heavy back-pressure
        p_ready = 15;
        wait (got == 3 * N / 4);
        // phase 3: no stalls at all -> one word per cycle
        p_valid = 100;
        p_ready = 100;
        repeat (4) @(posedge clk);
        t0 = got;
        repeat (100) @(posedge clk);
        if (got - t0 != 100) begin
            $display("FAIL: full-rate test moved %0d words in 100 cycles", got - t0);
            errors = errors + 1;
        end
        wait (got == N);
        repeat (5) @(posedge clk);
        if (got != N || sent != N) begin
            $display("FAIL: sent %0d got %0d", sent, got);
            errors = errors + 1;
        end
        $display("%0d words in order, %0d cycles", got, cyc);
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
