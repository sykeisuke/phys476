// top.v
module top(
    input  wire [2:0] in,
    output wire [2:0] out
);

    sub_module u0(.in(in[0]), .out(out[0]));
    sub_module u1(.in(in[1]), .out(out[1]));
    sub_module u2(.in(in[2]), .out(out[2]));

endmodule
