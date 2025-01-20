module encoder8to3 (
    input [7:0] A,    
    output [2:0] D
);

    assign D = (A[0] ? 3'b000 : 
                 (A[1] ? 3'b001 : 
                 (A[2] ? 3'b010 : 
                 (A[3] ? 3'b011 : 
                 (A[4] ? 3'b100 : 
                 (A[5] ? 3'b101 : 
                 (A[6] ? 3'b110 : 
                 (A[7] ? 3'b111 : 3'b000))))))));

endmodule

