module ethernet_frame (
    input wire clk,
    input wire start,
    output reg [7:0] eth_data,
    output reg eth_valid
);
    reg [3:0] state;

    always @(posedge clk) begin
        if (start) begin
            case (state)
                0:  begin eth_data <= 8'hAA; eth_valid <= 1; state <= 1; end // Destination MAC
                1:  begin eth_data <= 8'hBB; state <= 2; end
                2:  begin eth_data <= 8'hCC; state <= 3; end
                3:  begin eth_data <= 8'hDD; state <= 4; end
                4:  begin eth_data <= 8'hEE; state <= 5; end
                5:  begin eth_data <= 8'hFF; state <= 6; end
                6:  begin eth_data <= 8'h12; state <= 7; end // Source MAC
                7:  begin eth_data <= 8'h34; state <= 8; end
                8:  begin eth_data <= 8'h56; state <= 9; end
                9:  begin eth_data <= 8'h78; state <= 10; end
                10: begin eth_data <= 8'h9A; state <= 11; end
                11: begin eth_data <= 8'hBC; state <= 12; end
                12: begin eth_valid <= 0; state <= 0; end
            endcase
        end
    end
endmodule
