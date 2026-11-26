`timescale 1ns / 1ps
`default_nettype none

// Sends one UDP datagram (8-byte header + "Hello World") as a byte stream.
// One byte per clock while valid = 1; last = 1 on the final byte.
// The UDP length is 8 + 11 = 19 = 0x0013; the checksum is 0 (not used).
module udp_sender #(
    parameter [15:0] SRC_PORT = 16'd5000,
    parameter [15:0] DST_PORT = 16'd5001
) (
    input  wire       clk,
    input  wire       rst,          // synchronous, active high
    input  wire       start,        // one-cycle pulse: send a datagram
    output reg  [7:0] data,
    output reg        valid,
    output reg        last
);
    localparam N_PAY = 11;
    localparam N_ALL = 8 + N_PAY;
    localparam [15:0] LEN = N_ALL;

    reg [4:0] idx;                  // byte number 0 .. N_ALL-1
    reg       busy;

    function [7:0] byte_at(input [4:0] i);
        case (i)
            5'd0:  byte_at = SRC_PORT[15:8];
            5'd1:  byte_at = SRC_PORT[7:0];
            5'd2:  byte_at = DST_PORT[15:8];
            5'd3:  byte_at = DST_PORT[7:0];
            5'd4:  byte_at = LEN[15:8];
            5'd5:  byte_at = LEN[7:0];
            5'd6:  byte_at = 8'h00;           // checksum (MSB)
            5'd7:  byte_at = 8'h00;           // checksum (LSB)
            5'd8:  byte_at = "H";
            5'd9:  byte_at = "e";
            5'd10: byte_at = "l";
            5'd11: byte_at = "l";
            5'd12: byte_at = "o";
            5'd13: byte_at = " ";
            5'd14: byte_at = "W";
            5'd15: byte_at = "o";
            5'd16: byte_at = "r";
            5'd17: byte_at = "l";
            5'd18: byte_at = "d";
            default: byte_at = 8'h00;
        endcase
    endfunction

    always @(posedge clk) begin
        if (rst) begin
            busy  <= 1'b0;
            idx   <= 5'd0;
            valid <= 1'b0;
            last  <= 1'b0;
            data  <= 8'h00;
        end else if (!busy) begin
            valid <= 1'b0;
            last  <= 1'b0;
            if (start) begin
                busy <= 1'b1;
                idx  <= 5'd0;
            end
        end else begin
            data  <= byte_at(idx);
            valid <= 1'b1;
            last  <= (idx == N_ALL - 1);
            if (idx == N_ALL - 1)
                busy <= 1'b0;
            else
                idx <= idx + 1'b1;
        end
    end
endmodule

`default_nettype wire
