`timescale 1ns / 1ps
`default_nettype none

// Two-entry AXI-Stream skid buffer (register slice).
// s_tready comes straight from a flip-flop, so the ready path
// is cut as well as the data path. Full rate: one word per cycle.
module axis_skid #(
    parameter W = 8
) (
    input  wire         clk,
    input  wire         rst,          // synchronous, active high
    // slave side (from the source)
    input  wire         s_tvalid,
    output wire         s_tready,
    input  wire [W-1:0] s_tdata,
    // master side (to the sink)
    output reg          m_tvalid,
    input  wire         m_tready,
    output reg  [W-1:0] m_tdata
);
    reg         skid_valid;
    reg [W-1:0] skid_data;

    assign s_tready = ~skid_valid;    // ready while the skid slot is empty

    always @(posedge clk) begin
        if (rst) begin
            m_tvalid   <= 1'b0;
            skid_valid <= 1'b0;
        end else if (m_tready || !m_tvalid) begin
            // output register is free (or being emptied this edge)
            if (skid_valid) begin
                m_tdata    <= skid_data;
                m_tvalid   <= 1'b1;
                skid_valid <= 1'b0;
            end else begin
                m_tvalid <= s_tvalid;
                if (s_tvalid)
                    m_tdata <= s_tdata;
            end
        end else if (s_tvalid && s_tready) begin
            // output stalled: park the accepted word in the skid slot
            skid_data  <= s_tdata;
            skid_valid <= 1'b1;
        end
    end
endmodule

`default_nettype wire
