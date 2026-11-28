`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for axis_pedsub: random valid and ready,
// checks every output word and tlast in order. 200 words = 12 packets of
// 16 plus a short last packet of 8, so the last word (199) carries tlast,
// as the last word of every DMA transfer does.
module tb_axis_pedsub;
    localparam N = 200;
    reg         aclk = 1'b0, aresetn = 1'b0;
    reg  [31:0] s_tdata = 32'd0;
    reg         s_tlast = 1'b0, s_tvalid = 1'b0, m_tready = 1'b0;
    wire        s_tready, m_tlast, m_tvalid;
    wire [31:0] m_tdata;

    axis_pedsub #(.PED(32'd100)) dut (
        .aclk(aclk), .aresetn(aresetn),
        .s_axis_tdata(s_tdata), .s_axis_tlast(s_tlast),
        .s_axis_tvalid(s_tvalid), .s_axis_tready(s_tready),
        .m_axis_tdata(m_tdata), .m_axis_tlast(m_tlast),
        .m_axis_tvalid(m_tvalid), .m_axis_tready(m_tready));

    always #5 aclk = ~aclk;

    integer sent = 0, rcvd = 0, errors = 0, seed = 21, cycles = 0;
    reg     last_seen = 1'b0;               // tlast of the last word received

    // tlast is expected at the end of every 16-word packet and on word N-1
    function want_last(input integer k);
        want_last = ((k % 16) == 15) || (k == N - 1);
    endfunction

    always @(posedge aclk) begin
        cycles <= cycles + 1;
        // check the output side
        if (aresetn && m_tvalid && m_tready) begin
            if (m_tdata !== 1000 + rcvd - 100 || m_tlast !== want_last(rcvd)) begin
                $display("FAIL: word %0d data %0d last %b", rcvd, m_tdata, m_tlast);
                errors = errors + 1;
            end
            last_seen = m_tlast;
            rcvd = rcvd + 1;
        end
        // drive the input side: data 1000, 1001, ... ; packets of 16 words,
        // the last packet ends at word N-1
        if (aresetn && s_tvalid && s_tready) sent = sent + 1;
        if (aresetn && (!s_tvalid || s_tready)) begin
            s_tvalid <= (sent < N) && ($random(seed) % 4 != 0);
            s_tdata  <= 1000 + sent;
            s_tlast  <= want_last(sent);
        end
        m_tready <= ($random(seed) % 3 != 0);
    end

    initial begin
        repeat (4) @(posedge aclk);
        aresetn <= 1'b1;
        wait (rcvd == N || cycles > 5000);
        @(posedge aclk);
        if (rcvd != N) begin
            $display("FAIL: received %0d of %0d words", rcvd, N); errors = errors + 1;
        end else if (last_seen !== 1'b1) begin
            $display("FAIL: no tlast on the final word %0d", N - 1); errors = errors + 1;
        end
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
