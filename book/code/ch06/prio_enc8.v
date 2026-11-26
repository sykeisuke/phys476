`timescale 1ns / 1ps
`default_nettype none

// 8-to-3 priority encoder. 'idx' is the number of the highest input
// that is 1. 'valid' is 0 when no input is 1 (then idx = 0).
module prio_enc8 (
    input  wire [7:0] in,
    output reg  [2:0] idx,
    output wire       valid
);
    assign valid = |in;

    always @(*) begin
        casez (in)
            8'b1???????: idx = 3'd7;
            8'b01??????: idx = 3'd6;
            8'b001?????: idx = 3'd5;
            8'b0001????: idx = 3'd4;
            8'b00001???: idx = 3'd3;
            8'b000001??: idx = 3'd2;
            8'b0000001?: idx = 3'd1;
            default:     idx = 3'd0;
        endcase
    end
endmodule

`default_nettype wire
