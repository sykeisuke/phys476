module mdio_ctrl (
    input wire clk,       // システムクロック
    input wire rst,       // リセット
    output reg mdc,       // MDIO クロック
    inout wire mdio,      // MDIO データ線
    output reg ready      // 設定完了フラグ
);

    reg [31:0] shift_reg;
    reg mdio_out;
    reg mdio_oe;
    
    assign mdio = (mdio_oe) ? mdio_out : 1'bz;

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            mdio_oe <= 1;
            shift_reg <= 32'h00002000; // Auto-Negotiation
            ready <= 0;
        end else begin
            if (shift_reg != 0) begin
                mdio_out <= shift_reg[31];
                shift_reg <= shift_reg << 1;
            end else begin
                ready <= 1;
            end
        end
    end
endmodule
