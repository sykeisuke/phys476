module ip_header (
    input wire clk,
    input wire start,
    output reg [7:0] ip_data,
    output reg ip_valid
);

    reg [4:0] state;

    always @(posedge clk) begin
        if (start) begin
            case (state)
                0:  begin ip_data <= 8'h45; ip_valid <= 1; state <= 1; end // Version + IHL
                1:  begin ip_data <= 8'h00; state <= 2; end // TOS
                2:  begin ip_data <= 8'h00; state <= 3; end // Total Length (High)
                3:  begin ip_data <= 8'h27; state <= 4; end // Total Length (Low)
                4:  begin ip_data <= 8'h00; state <= 5; end // ID (High)
                5:  begin ip_data <= 8'h00; state <= 6; end // ID (Low)
                6:  begin ip_data <= 8'h40; state <= 7; end // Flags/Fragment Offset (High)
                7:  begin ip_data <= 8'h00; state <= 8; end // Flags/Fragment Offset (Low)
                8:  begin ip_data <= 8'h40; state <= 9; end // TTL
                9:  begin ip_data <= 8'h11; state <= 10; end // Protocol
                10: begin ip_data <= 8'hB6; state <= 11; end // Header Checksum (High)
                11: begin ip_data <= 8'hDF; state <= 12; end // Header Checksum (Low)
                12: begin ip_data <= 8'hC0; state <= 13; end // Source IP (192)
                13: begin ip_data <= 8'hA8; state <= 14; end // Source IP (168)
                14: begin ip_data <= 8'h01; state <= 15; end // Source IP (1)
                15: begin ip_data <= 8'h32; state <= 16; end // Source IP (50)
                16: begin ip_data <= 8'hC0; state <= 17; end // Destination IP (192)
                17: begin ip_data <= 8'hA8; state <= 18; end // Destination IP (168)
                18: begin ip_data <= 8'h01; state <= 19; end // Destination IP (1)
                19: begin ip_data <= 8'h64; state <= 20; end // Destination IP (100)
                20: begin ip_valid <= 0; state <= 0; end
            endcase
        end
    end
endmodule

