module udp_sender (
    input wire clk,
    input wire start,
    output reg [7:0] udp_data,
    output reg udp_valid
);

    reg [4:0] state;
    
    always @(posedge clk) begin
        if (start) begin
            case (state)
                // UDP Header (8 Bytes)
                0: begin udp_data <= 8'h13; udp_valid <= 1; state <= 1; end // Source Port (5000) [MSB]
                1: begin udp_data <= 8'h88; state <= 2; end // Source Port (5000) [LSB]
                2: begin udp_data <= 8'h13; state <= 3; end // Destination Port (5001) [MSB]
                3: begin udp_data <= 8'h89; state <= 4; end // Destination Port (5001) [LSB]
                4: begin udp_data <= 8'h00; state <= 5; end // Length (MSB) - 20 Bytes
                5: begin udp_data <= 8'h14; state <= 6; end // Length (LSB)
                6: begin udp_data <= 8'h00; state <= 7; end // Checksum (MSB)
                7: begin udp_data <= 8'h00; state <= 8; end // Checksum (LSB)

                // UDP Payload ("Hello World")
                8: begin udp_data <= 8'h48; state <= 9; end // 'H'
                9: begin udp_data <= 8'h65; state <= 10; end // 'e'
                10: begin udp_data <= 8'h6C; state <= 11; end // 'l'
                11: begin udp_data <= 8'h6C; state <= 12; end // 'l'
                12: begin udp_data <= 8'h6F; state <= 13; end // 'o'
                13: begin udp_data <= 8'h20; state <= 14; end // ' '
                14: begin udp_data <= 8'h57; state <= 15; end // 'W'
                15: begin udp_data <= 8'h6F; state <= 16; end // 'o'
                16: begin udp_data <= 8'h72; state <= 17; end // 'r'
                17: begin udp_data <= 8'h6C; state <= 18; end // 'l'
                18: begin udp_data <= 8'h64; state <= 19; end // 'd'
                19: begin udp_valid <= 0; state <= 0; end // 終了
            endcase
        end
    end
endmodule

