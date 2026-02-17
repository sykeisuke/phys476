module pipeline (
    input  wire        clk,
    input  wire        rst,
    input  wire        in_valid,
    input  wire signed [15:0] X,
    output reg         out_valid,
    output reg  signed [15:0] Y
);

    reg signed [15:0] s1, s2, s3;
    reg v1, v2, v3;

    always @(posedge clk) begin
        if (rst) begin
            s1 <= 0, s2 <= 0, s3 <=0;
            v1 <= 0, v2 <= 0, v3 <=0;
            Y <= 0;
            out_valid <= 0;
        end
        else begin
            v1 <= in_valid;
            v2 <= v1;
            v3 <= v2;
            out_valid <= v3;

            if (in_valid)
                s1 <= X * 3;
            if (v1)
                s2 <= s1 + 5;
            if (v2)
                s3 <= s2 * 2 + 7;
            if (v3)
                Y <= s3;
        end
    end
endmodule

