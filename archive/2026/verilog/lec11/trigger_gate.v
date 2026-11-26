module trigger_gate (
    input        TRIG,
    input  [7:0] DIN,
    output [7:0] DOUT
);

    assign DOUT = TRIG ? DIN : 8'd0;

endmodule

