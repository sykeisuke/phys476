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
                1:  begin ip_data <= 8'h00; state <= 2; end                // TOS
                2:  begin ip_data <= 8'h00; state <= 3; end                // Total Length (High)
                3:  begin ip_data <= 8'h36; state <= 4; end                // Total Length (Low)
                4:  begin ip_data <= 8'h00; state <= 5; end                // ID (High)
                5:  begin ip_data <= 8'h00; state <= 6; end                // ID (Low)
                6:  begin ip_data <= 8'h40; state <= 7; end                // Flags/Fragment Offset (High)
                7:  begin ip_data <= 8'h00; state <= 8; end                // Flags/Fragment Offset (Low)
                8:  begin ip_data <= 8'h40; state <= 9; end                // TTL
                9:  begin ip_data <= 8'h11; state <= 10; end               // Protocol (UDP)
                10: begin ip_data <= 8'h3E; state <= 11; end               // Header Checksum (High)
                11: begin ip_data <= 8'hE0; state <= 12; end               // Header Checksum (Low)
                12: begin ip_data <= 8'd169; state <= 13; end              // Source IP (169)
                13: begin ip_data <= 8'd254; state <= 14; end              // Source IP (254)
                14: begin ip_data <= 8'd1; state <= 15; end                // Source IP (1)
                15: begin ip_data <= 8'd3; state <= 16; end                // Source IP (3)
                16: begin ip_data <= 8'd169; state <= 17; end              // Destination IP (169)
                17: begin ip_data <= 8'd254; state <= 18; end              // Destination IP (254)
                18: begin ip_data <= 8'd28; state <= 19; end               // Destination IP (28)
                19: begin ip_data <= 8'd214; state <= 20; end              // Destination IP (214)
                20: begin ip_valid <= 0; state <= 0; end
            endcase
        end
    end
endmodule

