module rgmii_tx (
    input wire tx_clk,
    input wire [7:0] tx_data,
    input wire tx_valid,
    output reg tx_ctl,
    output wire [3:0] txd
);

    reg [3:0] txd_low;
    reg [3:0] txd_high;

    always @(posedge tx_clk) begin
        if (tx_valid) begin
            txd_low <= tx_data[3:0];
            tx_ctl <= 1'b1;
        end else begin
            txd_low <= 4'b0000;
            tx_ctl <= 1'b0;
        end
    end

    always @(negedge tx_clk) begin
        if (tx_valid) begin
            txd_high <= tx_data[7:4];
        end
    end

    assign txd = (tx_clk) ? txd_low : txd_high;

endmodule
