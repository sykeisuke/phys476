module non_pipeline (
    input  wire        clk,
    input  wire        rst,
    input  wire        in_valid,
    input  wire signed [15:0] X,
    output reg         out_valid,
    output reg  signed [15:0] Y
);

    wire signed [15:0] comb_y;
    assign comb_y = (X * 3 + 5) * 2 + 7;

    always @(posedge clk) begin
        if (rst) begin
            Y <= 0;
            out_valid <= 0;
        end
        else begin
            out_valid <= in_valid;
            if (in_valid)
                Y <= comb_y;
        end
    end

endmodule

