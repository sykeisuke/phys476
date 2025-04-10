module mdio_ctrl (
    input wire clk,       // System Clock 
    input wire rst,       // Reset
    output reg mdc,       // MDIO Clock
    inout wire mdio,      // MDIO Data line
    output reg ready      // READY when configuration is done
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
