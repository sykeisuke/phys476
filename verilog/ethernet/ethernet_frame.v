module ethernet_frame (
    input wire clk,
    input wire start,
    output reg [7:0] eth_data,
    output reg eth_valid
);

    reg [3:0] state;

    // Destination MAC (PCなど受信側のMACアドレス)
    parameter [47:0] dest_mac = 48'hAA_BB_CC_DD_EE_FF; 

    // Source MAC (FPGAのMACアドレス)
    parameter [47:0] src_mac  = 48'h00_0A_35_01_02_03; // EEPROMが未設定なら手動設定

    always @(posedge clk) begin
        if (start) begin
            case (state)
                // Destination MAC (6 bytes)
                0:  begin eth_data <= dest_mac[47:40]; eth_valid <= 1; state <= 1; end
                1:  begin eth_data <= dest_mac[39:32]; state <= 2; end
                2:  begin eth_data <= dest_mac[31:24]; state <= 3; end
                3:  begin eth_data <= dest_mac[23:16]; state <= 4; end
                4:  begin eth_data <= dest_mac[15:8];  state <= 5; end
                5:  begin eth_data <= dest_mac[7:0];   state <= 6; end

                // Source MAC (6 bytes)
                6:  begin eth_data <= src_mac[47:40]; state <= 7; end
                7:  begin eth_data <= src_mac[39:32]; state <= 8; end
                8:  begin eth_data <= src_mac[31:24]; state <= 9; end
                9:  begin eth_data <= src_mac[23:16]; state <= 10; end
                10: begin eth_data <= src_mac[15:8];  state <= 11; end
                11: begin eth_data <= src_mac[7:0];   state <= 12; end
                
                // 完了
                12: begin eth_valid <= 0; state <= 0; end
            endcase
        end
    end
endmodule
