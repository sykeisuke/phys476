module ip_header (
    input wire clk,
    input wire start,
    output reg [7:0] ip_data,
    output reg ip_valid
);
    reg [3:0] state;

    always @(posedge clk) begin
        if (start) begin
            case (state)
                0:  begin ip_data <= 8'h45; ip_valid <= 1; state <= 1; end // IPv4, 20 Byte Header
                1:  begin ip_data <= 8'h00; state <= 2; end // TOS
                2:  begin ip_data <= 8'h00; state <= 3; end // Total Length (High)
                3:  begin ip_data <= 8'h2E; state <= 4; end // Total Length (Low)
                4:  begin ip_data <= 8'h00; state <= 5; end // Identification
                5:  begin ip_data <= 8'h00; state <= 6; end
                6:  begin ip_data <= 8'h40; state <= 7; end // Flags
                7:  begin ip_data <= 8'h00; state <= 8; end
                8:  begin ip_data <= 8'h40; state <= 9; end // TTL
                9:  begin ip_data <= 8'h11; state <= 10; end // Protocol (UDP)
                10: begin ip_data <= 8'h00; state <= 11; end // Header Checksum
                11: begin ip_data <= 8'h00; state <= 12; end
                12: begin ip_data <= 8'hC0; state <= 13; end // Source IP (192.168.1.50)
                13: begin ip_data <= 8'hA8; state <= 14; end
                14: begin ip_data <= 8'h01; state <= 15; end
                15: begin ip_data <= 8'h32; state <= 16; end
                16: begin ip_data <= 8'hC0; state <= 17; end // Destination IP (192.168.1.100)
                17: begin ip_data <= 8'hA8; state <= 18; end
                18: begin ip_data <= 8'h01; state <= 19; end
                19: begin ip_data <= 8'h64; state <= 20; end
                20: begin ip_valid <= 0; state <= 0; end
            endcase
        end
    end
endmodule
