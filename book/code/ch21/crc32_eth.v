`timescale 1ns / 1ps
`default_nettype none

// Ethernet CRC-32 (the frame check sequence), one byte per clock.
// Reflected polynomial 0xEDB88320, start value 0xFFFFFFFF, final inversion.
// The FCS is sent least significant byte first: fcs[7:0] goes out first.
module crc32_eth (
    input  wire        clk,
    input  wire        init,      // start a new frame
    input  wire        en,        // data is a byte of the frame
    input  wire [7:0]  data,
    output wire [31:0] fcs        // ~crc: valid after the last byte
);
    reg [31:0] crc;

    function [31:0] next_crc(input [31:0] c, input [7:0] d);
        integer k;
        begin
            next_crc = c ^ {24'd0, d};
            for (k = 0; k < 8; k = k + 1)
                next_crc = next_crc[0] ? (next_crc >> 1) ^ 32'hEDB88320
                                       : (next_crc >> 1);
        end
    endfunction

    always @(posedge clk) begin
        if (init)
            crc <= 32'hFFFFFFFF;
        else if (en)
            crc <= next_crc(crc, data);
    end

    assign fcs = ~crc;
endmodule

`default_nettype wire
