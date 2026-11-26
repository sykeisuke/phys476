`timescale 1ns / 1ps
`default_nettype none

// CMOS gates described transistor by transistor with Verilog switch primitives.
// nmos (out, in, gate) conducts when gate = 1; pmos conducts when gate = 0.
// For simulation only: synthesis tools build gates from LUTs, not from these.
module cmos_inv (
    input  wire a,
    output wire y
);
    supply1 vdd;
    supply0 gnd;
    pmos p1 (y, vdd, a);    // pull-up
    nmos n1 (y, gnd, a);    // pull-down
endmodule

module cmos_nand2 (
    input  wire a, b,
    output wire y
);
    supply1 vdd;
    supply0 gnd;
    wire    mid;            // node between the two series nMOS
    pmos p1 (y, vdd, a);    // pMOS in parallel
    pmos p2 (y, vdd, b);
    nmos n1 (y, mid, a);    // nMOS in series
    nmos n2 (mid, gnd, b);
endmodule

module cmos_nor2 (
    input  wire a, b,
    output wire y
);
    supply1 vdd;
    supply0 gnd;
    wire    mid;            // node between the two series pMOS
    pmos p1 (mid, vdd, a);  // pMOS in series
    pmos p2 (y, mid, b);
    nmos n1 (y, gnd, a);    // nMOS in parallel
    nmos n2 (y, gnd, b);
endmodule

`default_nettype wire
