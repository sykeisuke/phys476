module tff_chain (
    input CLK,
    output wire Q1,
    output wire Q2,
    output wire Q3
);
    wire t1, t2;

    tff FF1 (.CLK(CLK), .T(1'b1), .Q(t1));
    tff FF2 (.CLK(t1), .T(1'b1), .Q(t2));
    tff FF3 (.CLK(t2), .T(1'b1), .Q(Q3));

    assign Q1 = t1;
    assign Q2 = t2;

endmodule
