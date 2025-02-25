module rgmii_tx (
    input wire tx_clk,
    input wire [7:0] tx_data,
    input wire tx_valid,
    output reg [3:0] txd,
    output reg tx_ctl
);

    always @(posedge tx_clk) begin
        if (tx_valid) begin
            txd <= tx_data[3:0];
            tx_ctl <= 1'b1;
        end else begin
            txd <= 4'b0000;
            tx_ctl <= 1'b0;
        end
    end

    always @(negedge tx_clk) begin
        if (tx_valid) begin
            txd <= tx_data[7:4];
        end
    end

endmodule
