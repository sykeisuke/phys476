`timescale 1ns / 1ps
`default_nettype none

// IPv4 header checksum (RFC 791): one's-complement sum of the ten 16-bit
// words of the 20-byte header, then inverted.
// To make the checksum: set the checksum field (bytes 10-11) to 0.
// To check a received header: the result is 0 when the header is good.
module ip_checksum (
    input  wire [159:0] hdr,      // byte 0 in hdr[159:152]
    output reg  [15:0]  csum
);
    reg [19:0] sum;               // 10 words of 16 bits fit in 20 bits
    integer i;

    always @(*) begin
        sum = 20'd0;
        for (i = 0; i < 10; i = i + 1)
            sum = sum + hdr[159 - 16*i -: 16];
        sum  = {4'd0, sum[15:0]} + {16'd0, sum[19:16]};   // fold carries
        sum  = {4'd0, sum[15:0]} + {16'd0, sum[19:16]};   // fold again
        csum = ~sum[15:0];
    end
endmodule

`default_nettype wire
