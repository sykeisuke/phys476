module comparator_gt (
    input  [9:0] A,
    input  [9:0] TH,
    output       GT
);
    assign GT = (A > TH);
endmodule
