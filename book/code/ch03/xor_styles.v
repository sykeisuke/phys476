`timescale 1ns / 1ps
`default_nettype none

// One function, five descriptions: all five modules are a 2-input XOR.
// Synthesis maps every one of them to the same single LUT.

// 1. Operator
module xor_op (
    input  wire a, b,
    output wire y
);
    assign y = a ^ b;
endmodule

// 2. NAND gates only (four NANDs, see Chapter 2)
module xor_nand (
    input  wire a, b,
    output wire y
);
    wire n1 = ~(a & b);
    wire n2 = ~(a & n1);
    wire n3 = ~(b & n1);
    assign y = ~(n2 & n3);
endmodule

// 3. Truth table with the conditional operator
module xor_table (
    input  wire a, b,
    output wire y
);
    wire [1:0] ab = {a, b};
    assign y = (ab == 2'b01) ? 1'b1 :
               (ab == 2'b10) ? 1'b1 : 1'b0;
endmodule

// 4. Truth table with case inside always @(*)
module xor_case (
    input  wire a, b,
    output reg  y
);
    always @(*) begin
        case ({a, b})
            2'b01:   y = 1'b1;
            2'b10:   y = 1'b1;
            default: y = 1'b0;
        endcase
    end
endmodule

// 5. Hierarchy: instantiate another module, ports connected by name
module xor_inst (
    input  wire a, b,
    output wire y
);
    xor_op u_xor (.a(a), .b(b), .y(y));
endmodule

`default_nettype wire
