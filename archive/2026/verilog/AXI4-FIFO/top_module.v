module top_module (
    input  wire        aclk,
    input  wire        aresetn,

    // AXI-Stream input (Source -> FIFO)
    input  wire [7:0]  s_axis_tdata,
    input  wire        s_axis_tvalid,
    output wire        s_axis_tready,

    // AXI-Stream output (FIFO -> Sink)
    output wire [7:0]  m_axis_tdata,
    output wire        m_axis_tvalid,
    input  wire        m_axis_tready
);

  axis_data_fifo_0 u_fifo (
    .s_axis_aresetn(aresetn),
    .s_axis_aclk   (aclk),

    .s_axis_tvalid (s_axis_tvalid),
    .s_axis_tready (s_axis_tready),
    .s_axis_tdata  (s_axis_tdata),

    .m_axis_tvalid (m_axis_tvalid),
    .m_axis_tready (m_axis_tready),
    .m_axis_tdata  (m_axis_tdata)
  );

endmodule
