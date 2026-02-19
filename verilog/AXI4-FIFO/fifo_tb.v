`timescale 1ns/1ps

module fifo_tb;

  reg clk = 0;
  always #5 clk = ~clk;  // 100MHz

  reg aresetn = 0;

  // Source -> FIFO
  reg  [7:0] s_tdata  = 0;
  reg        s_tvalid = 0;
  wire       s_tready;

  // FIFO -> Sink
  wire [7:0] m_tdata;
  wire       m_tvalid;
  reg        m_tready = 1;

  top_module inst (
    .aclk(clk),
    .aresetn(aresetn),

    .s_axis_tdata (s_tdata),
    .s_axis_tvalid(s_tvalid),
    .s_axis_tready(s_tready),

    .m_axis_tdata (m_tdata),
    .m_axis_tvalid(m_tvalid),
    .m_axis_tready(m_tready)
  );

  integer k;

  initial begin
    // reset
    repeat (4) @(posedge clk);
    aresetn <= 1;
    s_tvalid <= 1;

    // 50 cycles run, stall in the middle
    for (k = 0; k < 50; k = k + 1) begin
      @(posedge clk);

      // stall window (backpressure)
      if (k == 15) m_tready <= 0;
      if (k == 25) m_tready <= 1;

      // increment data only when accepted by FIFO
      if (s_tvalid && s_tready)
        s_tdata <= s_tdata + 1;
    end

    // stop
    s_tvalid <= 0;
    repeat (10) @(posedge clk);
    $finish;
  end

endmodule
