`timescale 1ns / 1ps
`default_nettype none

// Icarus-only stand-in for the Vivado IP axis_data_fifo_0 (Lab 4), so
// that fifo_tb.v can be tried without Vivado. A 16-word FIFO with
// AXI4-Stream ports; the word at the head is shown on m_axis_tdata.
// Do NOT add this file to the Vivado project: there the generated IP
// takes its place. Latency and depth differ from the real IP.
module axis_data_fifo_0 (
    input  wire       s_axis_aresetn,
    input  wire       s_axis_aclk,
    input  wire       s_axis_tvalid,
    output wire       s_axis_tready,
    input  wire [7:0] s_axis_tdata,
    output wire       m_axis_tvalid,
    input  wire       m_axis_tready,
    output wire [7:0] m_axis_tdata
);
    reg [7:0] mem [0:15];
    reg [4:0] wp = 0, rp = 0;         // one extra bit: full vs empty
    wire empty = (wp == rp);
    wire full  = (wp[4] != rp[4]) && (wp[3:0] == rp[3:0]);

    assign s_axis_tready = s_axis_aresetn && !full;
    assign m_axis_tvalid = !empty;
    assign m_axis_tdata  = mem[rp[3:0]];

    always @(posedge s_axis_aclk) begin
        if (!s_axis_aresetn) begin
            wp <= 0;
            rp <= 0;
        end else begin
            if (s_axis_tvalid && s_axis_tready) begin
                mem[wp[3:0]] <= s_axis_tdata;
                wp <= wp + 1'b1;
            end
            if (m_axis_tvalid && m_axis_tready)
                rp <= rp + 1'b1;
        end
    end
endmodule

`default_nettype wire
