`timescale 1ns/1ps

module fifo_tb;

  reg clk = 0;
  always #5 clk = ~clk; 

  reg aresetn = 0;

  // Source -> FIFO
  reg  [7:0] s_tdata;
  reg        s_tvalid;
  wire       s_tready;

  // FIFO -> Sink
  wire [7:0] m_tdata;
  wire       m_tvalid;
  reg        m_tready;

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

  integer send_cnt;
  integer recv_cnt;

  wire s_fire = s_tvalid && s_tready;
  wire m_fire = m_tvalid && m_tready;

  initial begin
    s_tdata  = 0;
    s_tvalid = 0;
    s_tlast  = 0;
    m_tready = 0;
    send_cnt = 0;
    recv_cnt = 0;

    // reset
    repeat (4) @(posedge clk);
    aresetn <= 1;

    @(posedge clk);
    m_tready <= 1;

    fork
      begin : SOURCE_PROC
        integer i;
        for (i = 0; i < 20; i = i + 1) begin
          @(posedge clk);
          s_tdata  <= i[7:0];
          s_tvalid <= 1;
          s_tlast  <= (i == 19);

          while (!(s_tvalid && s_tready)) @(posedge clk);

          @(posedge clk);
          s_tvalid <= 0;
          s_tlast  <= 0;

          if (i == 5 || i == 12) @(posedge clk);
        end
      end

      begin : SINK_STALL_PROC
        repeat (10) @(posedge clk);
        m_tready <= 0;   // stop
        repeat (8) @(posedge clk);
        m_tready <= 1;   // resume

        repeat (15) @(posedge clk);
        m_tready <= 0;   // stop again
        repeat (5) @(posedge clk);
        m_tready <= 1;   // resume
      end
    join

    repeat (30) @(posedge clk);
    $finish;
  end

  always @(posedge clk) begin
    if (!aresetn) begin
      send_cnt <= 0;
      recv_cnt <= 0;
    end else begin
      if (s_fire) send_cnt <= send_cnt + 1;
      if (m_fire) recv_cnt <= recv_cnt + 1;
    end
  end

endmodule
