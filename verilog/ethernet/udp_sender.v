module udp_sender (
    input wire clk,
    input wire start,
    output reg [7:0] udp_data,
    output reg udp_valid
);

    reg [3:0] state;
    
    always @(posedge clk) begin
        if (start) begin
            case (state)
                0: begin udp_data <= 8'h48; udp_valid <= 1; state <= 1; end // 'H'
                1: begin udp_data <= 8'h65; state <= 2; end // 'e'
                2: begin udp_data <= 8'h6C; state <= 3; end // 'l'
                3: begin udp_data <= 8'h6C; state <= 4; end // 'l'
                4: begin udp_data <= 8'h6F; state <= 5; end // 'o'
                5: begin udp_data <= 8'h20; state <= 6; end // ' '
                6: begin udp_data <= 8'h57; state <= 7; end // 'W'
                7: begin udp_data <= 8'h6F; state <= 8; end // 'o'
                8: begin udp_data <= 8'h72; state <= 9; end // 'r'
                9: begin udp_data <= 8'h6C; state <= 10; end // 'l'
                10: begin udp_data <= 8'h64; state <= 11; end // 'd'
                11: begin udp_valid <= 0; state <= 0; end // 終了
            endcase
        end
    end
endmodule
