module mux4 (
    input  [7:0] D0,
    input  [7:0] D1,
    input  [7:0] D2,
    input  [7:0] D3,
    input  [1:0] SEL,
    output [7:0] Y
);

    assign Y = (SEL == 2'd0) ? D0 :
               (SEL == 2'd1) ? D1 :
               (SEL == 2'd2) ? D2 :
                               D3;

endmodule

