`timescale 1ns / 1ps
`default_nettype none

// Full adder: adds two bits and a carry-in.
module full_adder (
    input  wire a, b, cin,
    output wire s,
    output wire cout
);
    assign s    = a ^ b ^ cin;
    assign cout = (a & b) | (cin & (a ^ b));
endmodule

`default_nettype wire
