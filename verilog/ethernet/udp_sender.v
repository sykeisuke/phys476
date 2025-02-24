module udp_sender (
    input wire clk,
    input wire send,
    output reg [7:0] tx_data,
    output reg tx_valid
);

    reg [3:0] state;
    
    always @(posedge clk) begin
        if (send) begin
            case (state)
                0: begin tx_data <= 8'h48; tx_valid <= 1; state <= 1; end // 'H'
                1: begin tx_data <= 8'h65; state <= 2; end // 'e'
                2: begin tx_data <= 8'h6C; state <= 3; end // 'l'
                3: begin tx_data <= 8'h6C; state <= 4; end // 'l'
                4: begin tx_data <= 8'h6F; state <= 5; end // 'o'
                5: begin tx_data <= 8'h20; state <= 6; end // ' '
                6: begin tx_data <= 8'h57; state <= 7; end // 'W'
                7: begin tx_data <= 8'h6F; state <= 8; end // 'o'
                8: begin tx_data <= 8'h72; state <= 9; end // 'r'
                9: begin tx_data <= 8'h6C; state <= 10; end // 'l'
                10: begin tx_data <= 8'h64; state <= 11; end // 'd'
                11: begin tx_valid <= 0; state <= 0; end // 終了
            endcase
        end
    end
endmodule
