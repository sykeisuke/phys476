`timescale 1ns / 1ps
`default_nettype none

// AXI-Stream block for a DMA loopback: subtracts a pedestal from every
// 32-bit word and passes tlast through. One register stage.
// Place it between the AXI DMA's MM2S (read) and S2MM (write) streams.
module axis_pedsub #(
    parameter [31:0] PED = 32'd100
) (
    input  wire        aclk,
    input  wire        aresetn,          // active low, as in AXI
    // slave side (from DMA MM2S)
    input  wire [31:0] s_axis_tdata,
    input  wire        s_axis_tlast,
    input  wire        s_axis_tvalid,
    output wire        s_axis_tready,
    // master side (to DMA S2MM)
    output reg  [31:0] m_axis_tdata,
    output reg         m_axis_tlast,
    output reg         m_axis_tvalid,
    input  wire        m_axis_tready
);
    // accept a new word when the output register is empty or being read
    assign s_axis_tready = ~m_axis_tvalid | m_axis_tready;

    always @(posedge aclk) begin
        if (!aresetn) begin
            m_axis_tvalid <= 1'b0;
            m_axis_tdata  <= 32'd0;
            m_axis_tlast  <= 1'b0;
        end else if (s_axis_tready) begin
            m_axis_tvalid <= s_axis_tvalid;
            m_axis_tdata  <= s_axis_tdata - PED;
            m_axis_tlast  <= s_axis_tlast;
        end
    end
endmodule

`default_nettype wire
