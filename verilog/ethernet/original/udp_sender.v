module udp_sender (
    input wire clk,
    input wire start,
    output reg [7:0] udp_data,
    output reg udp_valid
);

    reg [6:0] state;

    always @(posedge clk) begin
        if (start) begin
            case (state)
                // UDP Header (8 Bytes)
                0: begin udp_data <= 8'h13; udp_valid <= 1; state <= 1; end // Source Port (5000) [MSB]
                1: begin udp_data <= 8'h88; state <= 2; end                 // Source Port (5000) [LSB]
                2: begin udp_data <= 8'h13; state <= 3; end                 // Destination Port (5001) [MSB]
                3: begin udp_data <= 8'h89; state <= 4; end                 // Destination Port (5001) [LSB]
                4: begin udp_data <= 8'h00; state <= 5; end                 // Length (MSB) → 54byte = 0x0036
                5: begin udp_data <= 8'h36; state <= 6; end                 // Length (LSB)
                6: begin udp_data <= 8'h00; state <= 7; end                 // Checksum (MSB) 0
                7: begin udp_data <= 8'h00; state <= 8; end                 // Checksum (LSB)

                // UDP Payload ("Hello World")
                8: begin udp_data <= "H"; state <= 9; end
                9: begin udp_data <= "e"; state <= 10; end
                10: begin udp_data <= "l"; state <= 11; end
                11: begin udp_data <= "l"; state <= 12; end
                12: begin udp_data <= "o"; state <= 13; end
                13: begin udp_data <= " "; state <= 14; end
                14: begin udp_data <= "W"; state <= 15; end
                15: begin udp_data <= "o"; state <= 16; end
                16: begin udp_data <= "r"; state <= 17; end
                17: begin udp_data <= "l"; state <= 18; end
                18: begin udp_data <= "d"; state <= 19; end

                // Padding 0x00 (46 - 11 = 35byte)
                19: begin udp_data <= 8'h00; state <= 20; end
                20: begin udp_data <= 8'h00; state <= 21; end
                21: begin udp_data <= 8'h00; state <= 22; end
                22: begin udp_data <= 8'h00; state <= 23; end
                23: begin udp_data <= 8'h00; state <= 24; end
                24: begin udp_data <= 8'h00; state <= 25; end
                25: begin udp_data <= 8'h00; state <= 26; end
                26: begin udp_data <= 8'h00; state <= 27; end
                27: begin udp_data <= 8'h00; state <= 28; end
                28: begin udp_data <= 8'h00; state <= 29; end
                29: begin udp_data <= 8'h00; state <= 30; end
                30: begin udp_data <= 8'h00; state <= 31; end
                31: begin udp_data <= 8'h00; state <= 32; end
                32: begin udp_data <= 8'h00; state <= 33; end
                33: begin udp_data <= 8'h00; state <= 34; end
                34: begin udp_data <= 8'h00; state <= 35; end
                35: begin udp_data <= 8'h00; state <= 36; end
                36: begin udp_data <= 8'h00; state <= 37; end
                37: begin udp_data <= 8'h00; state <= 38; end
                38: begin udp_data <= 8'h00; state <= 39; end
                39: begin udp_data <= 8'h00; state <= 40; end
                40: begin udp_data <= 8'h00; state <= 41; end
                41: begin udp_data <= 8'h00; state <= 42; end
                42: begin udp_data <= 8'h00; state <= 43; end
                43: begin udp_data <= 8'h00; state <= 44; end
                44: begin udp_data <= 8'h00; state <= 45; end
                45: begin udp_data <= 8'h00; state <= 46; end
                46: begin udp_data <= 8'h00; state <= 47; end
                47: begin udp_data <= 8'h00; state <= 48; end
                48: begin udp_data <= 8'h00; state <= 49; end
                49: begin udp_data <= 8'h00; state <= 50; end
                50: begin udp_data <= 8'h00; state <= 51; end
                51: begin udp_data <= 8'h00; state <= 52; end
                52: begin udp_data <= 8'h00; state <= 53; end
                53: begin udp_data <= 8'h00; state <= 54; end
                54: begin udp_data <= 8'h00; state <= 55; end
                55: begin udp_data <= 8'h00; state <= 56; end
                56: begin udp_data <= 8'h00; state <= 57; end
                57: begin udp_data <= 8'h00; state <= 58; end

                // done
                58: begin udp_valid <= 0; state <= 0; end
            endcase
        end
    end
endmodule
