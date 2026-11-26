module roi_max_encoder (
    input  [7:0] E0,
    input  [7:0] E1,
    input  [7:0] E2,
    input  [7:0] E3,
    output [1:0] ROI
);

    reg [1:0] roi_r;

    always @(*) begin
        roi_r = 2'd0; // E0
        if (E1 > E0)
            roi_r = 2'd1;

        if (E2 > ((roi_r == 2'd0) ? E0 : E1))
            roi_r = 2'd2;

        if (E3 > ((roi_r == 2'd0) ? E0 :
                  (roi_r == 2'd1) ? E1 : E2))
            roi_r = 2'd3;
    end

    assign ROI = roi_r;

endmodule
