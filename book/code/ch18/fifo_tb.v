`timescale 1ns / 1ps
`default_nettype none

// Lab 4 testbench: drives top_module (AXI4-Stream Data FIFO IP) with a
// source that obeys the AXI-Stream rules and a sink that stalls for
// 10 cycles. Checks that every word leaves in order, that m_tdata is
// held while stalled, and counts how often s_tready was 0.
// Vivado: add as a simulation source, Run Behavioral Simulation, then
// "run all" in the Tcl console.
// Without Vivado (Icarus, with a stand-in for the IP):
//   iverilog -g2005 -Wall -o fifo_sim fifo_tb.v top_module.v axis_fifo_model.v
module fifo_tb;
    reg clk = 0;
    always #5 clk = ~clk;             // 100 MHz

    reg        aresetn  = 0;
    reg  [7:0] s_tdata  = 0;          // source -> FIFO
    reg        s_tvalid = 0;
    wire       s_tready;
    wire [7:0] m_tdata;               // FIFO -> sink
    wire       m_tvalid;
    reg        m_tready = 1;

    top_module dut (
        .aclk(clk), .aresetn(aresetn),
        .s_axis_tdata(s_tdata), .s_axis_tvalid(s_tvalid), .s_axis_tready(s_tready),
        .m_axis_tdata(m_tdata), .m_axis_tvalid(m_tvalid), .m_axis_tready(m_tready));

    integer k, n_in = 0, n_out = 0, n_full = 0, errors = 0;
    reg [7:0] expect_d = 0;
    reg       stalled  = 0;            // last edge had m_tvalid & !m_tready
    reg [7:0] held     = 0;

    // source: after reset, always valid; next word only after a transfer
    initial begin
        repeat (4) @(posedge clk);
        aresetn  <= 1'b1;
        s_tvalid <= 1'b1;
        for (k = 0; k < 50; k = k + 1) begin
            @(posedge clk);
            if (k == 15) m_tready <= 1'b0;    // sink stalls for 10 cycles
            if (k == 25) m_tready <= 1'b1;
            if (s_tvalid && s_tready) begin   // transfer at this edge
                s_tdata <= s_tdata + 1'b1;
                n_in = n_in + 1;
            end
        end
        s_tvalid <= 1'b0;
        repeat (20) @(posedge clk);           // let the FIFO drain
        if (n_out != n_in) begin
            $display("FAIL: sent %0d words, received %0d", n_in, n_out);
            errors = errors + 1;
        end
        if (errors == 0)
            $display("PASS (%0d words in order; s_tready was 0 in %0d cycles)", n_out, n_full);
        $finish;
    end

    // sink: check order and the hold rule
    always @(posedge clk) begin
        if (aresetn && s_tvalid && !s_tready) n_full = n_full + 1;
        if (stalled && m_tdata !== held) begin
            $display("FAIL: m_tdata changed while stalled (%h -> %h)", held, m_tdata);
            errors = errors + 1;
        end
        stalled <= m_tvalid && !m_tready;
        held    <= m_tdata;
        if (m_tvalid && m_tready) begin
            if (m_tdata !== expect_d) begin
                $display("FAIL: got %h, expected %h", m_tdata, expect_d);
                errors = errors + 1;
            end
            expect_d <= expect_d + 1'b1;
            n_out = n_out + 1;
        end
    end
endmodule

`default_nettype wire
