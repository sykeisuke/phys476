module ethernet_frame (
    input wire clk,
    input wire start,
    output reg [7:0] eth_data,
    output reg eth_valid
);

    reg [5:0] state;

    // Preamble & Start Frame Delimiter (SFD)
    parameter [63:0] preamble = 64'h55_55_55_55_55_55_55_D5; // 7 bytes Preamble + 1 byte SFD

    // Destination MAC (PC側のMACアドレスを指定)
    parameter [47:0] dest_mac = 48'hAA_BB_CC_DD_EE_FF; 

    // Source MAC (FPGAのMACアドレス)
    parameter [47:0] src_mac  = 48'h00_0A_35_01_02_03;

    // EtherType (IPv4)
    parameter [15:0] ether_type = 16'h0800;

    always @(posedge clk) begin
        if (start) begin
            case (state)
                // Preamble & SFD (8 bytes)
                0:  begin eth_data <= preamble[63:56]; eth_valid <= 1; state <= 1; end
                1:  begin eth_data <= preamble[55:48]; state <= 2; end
                2:  begin eth_data <= preamble[47:40]; state <= 3; end
                3:  begin eth_data <= preamble[39:32]; state <= 4; end
                4:  begin eth_data <= preamble[31:24]; state <= 5; end
                5:  begin eth_data <= preamble[23:16]; state <= 6; end
                6:  begin eth_data <= preamble[15:8];  state <= 7; end
                7:  begin eth_data <= preamble[7:0];   state <= 8; end

                // Destination MAC (6 bytes)
                8:  begin eth_data <= dest_mac[47:40]; state <= 9; end
                9:  begin eth_data <= dest_mac[39:32]; state <= 10; end
                10: begin eth_data <= dest_mac[31:24]; state <= 11; end
                11: begin eth_data <= dest_mac[23:16]; state <= 12; end
                12: begin eth_data <= dest_mac[15:8];  state <= 13; end
                13: begin eth_data <= dest_mac[7:0];   state <= 14; end

                // Source MAC (6 bytes)
                14: begin eth_data <= src_mac[47:40]; state <= 15; end
                15: begin eth_data <= src_mac[39:32]; state <= 16; end
                16: begin eth_data <= src_mac[31:24]; state <= 17; end
                17: begin eth_data <= src_mac[23:16]; state <= 18; end
                18: begin eth_data <= src_mac[15:8];  state <= 19; end
                19: begin eth_data <= src_mac[7:0];   state <= 20; end

                // EtherType (2 bytes)
                20: begin eth_data <= ether_type[15:8]; state <= 21; end
                21: begin eth_data <= ether_type[7:0];  state <= 22; end

                // 完了
                22: begin eth_valid <= 0; state <= 0; end
            endcase
        end
    end
endmodule
