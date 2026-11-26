`timescale 1ns / 1ps
`default_nettype none

// Trigger-gated ROI readout (reference solution of the Chapter 4 exercise).
// trig     = (e0 + e1 + e2 + e3 > th)
// roi      = sector with the largest energy (ties: lower index)
// data_out = trig ? energy of sector roi : 0
module roi_readout (
    input  wire [7:0] e0, e1, e2, e3,
    input  wire [9:0] th,
    output wire       trig,
    output wire [1:0] roi,
    output reg  [7:0] data_out
);
    wire [9:0] esum;

    calo_trigger u_trig (.e0(e0), .e1(e1), .e2(e2), .e3(e3),
                         .th(th), .esum(esum), .trig(trig));
    roi_max4 u_roi (.e0(e0), .e1(e1), .e2(e2), .e3(e3), .roi(roi));

    always @(*) begin
        case (roi)
            2'd0:    data_out = e0;
            2'd1:    data_out = e1;
            2'd2:    data_out = e2;
            default: data_out = e3;
        endcase
        if (!trig)
            data_out = 8'd0;
    end
endmodule

`default_nettype wire
