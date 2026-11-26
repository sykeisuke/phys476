`timescale 1ns / 1ps
`default_nettype none

// Cycle-by-cycle trace of axis_skid for one fixed back-pressure pattern.
// The source always has data (words 1, 2, 3, ...) and obeys the AXI-Stream
// rules. The table it prints is the "skid buffer occupancy" example.
// Self-check: the words that leave are 1, 2, 3, ... in order.
module tb_skid_trace;
    reg clk = 1'b0, rst = 1'b1;
    always #5 clk = ~clk;

    reg        s_tvalid = 1'b0;
    reg  [7:0] s_tdata  = 8'd1;
    wire       s_tready;
    wire       m_tvalid;
    wire [7:0] m_tdata;
    reg        m_tready = 1'b0;

    axis_skid #(.W(8)) dut (.clk(clk), .rst(rst), .s_tvalid(s_tvalid), .s_tready(s_tready),
                            .s_tdata(s_tdata), .m_tvalid(m_tvalid), .m_tready(m_tready),
                            .m_tdata(m_tdata));

    // m_tready per cycle: runs, a 2-cycle stall, then runs again
    reg [0:8] ready_pat = 9'b110011111;
    integer cyc, errors = 0;
    reg [7:0] expect_out = 8'd1;

    initial begin
        repeat (2) @(posedge clk);
        #1 rst = 1'b0;
        s_tvalid = 1'b1;                       // source has data from now on
        $display("cyc  m_tready | s_tready s_tdata | skid | m_tvalid m_tdata | moves");
        for (cyc = 0; cyc < 9; cyc = cyc + 1) begin
            m_tready = ready_pat[cyc];
            #3;                                // signals settled, before the edge
            $display("%2d      %b     |    %b       %2d    |  %b   |    %b       %2d   | %s%s",
                     cyc, m_tready, s_tready, s_tdata, dut.skid_valid, m_tvalid,
                     m_tvalid ? m_tdata : 8'd0,
                     (s_tvalid && s_tready) ? "in " : "   ",
                     (m_tvalid && m_tready) ? "out" : "");
            if (m_tvalid && m_tready) begin
                if (m_tdata !== expect_out) begin
                    $display("FAIL: out %0d, expected %0d", m_tdata, expect_out);
                    errors = errors + 1;
                end
                expect_out = expect_out + 1'b1;
            end
            @(posedge clk);
            if (s_tvalid && s_tready) #1 s_tdata = s_tdata + 1'b1;   // next word
            else #1;
        end
        if (errors == 0 && expect_out > 8'd5)
            $display("PASS (%0d words out in order)", expect_out - 1);
        else
            $display("FAIL");
        $finish;
    end
endmodule

`default_nettype wire
