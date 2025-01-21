module PriorityEncoder (
    input  [63:0] in,    // 64-bit input
    output reg [6:0] out // 7-bit output
);
    integer i;
    always @(*) begin
        out = 7'b0000000; // Default output
        for (i = 63; i >= 0; i = i - 1) begin
            if (in[i]) begin
                out = i;  
                break;    
            end
        end
    end
endmodule
