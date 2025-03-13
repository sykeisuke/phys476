module crc32_ethernet (
    input wire clk,
    input wire rst,
    input wire data_valid,
    input wire [7:0] data_in,
    input wire crc_init,       // 初期化（パケット開始時1クロックだけHigh）
    input wire crc_calc,       // データ送信時に1
    input wire crc_finish,     // CRCを読み出したいときに1
    output reg [31:0] crc_out, // CRCの値
    output reg crc_valid       // CRC有効信号
);
    reg [31:0] crc_reg;
    wire [31:0] next_crc;

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            crc_reg <= 32'hFFFFFFFF;
            crc_out <= 32'hFFFFFFFF;
            crc_valid <= 0;
        end else begin
            if (crc_init) begin
                crc_reg <= 32'hFFFFFFFF;
            end else if (crc_calc && data_valid) begin
                crc_reg <= next_crc;
            end else if (crc_finish) begin
                crc_out <= ~crc_reg;
                crc_valid <= 1;
            end else begin
                crc_valid <= 0;
            end
        end
    end

    assign next_crc = crc32_byte(crc_reg, data_in);

    function [31:0] crc32_byte;
        input [31:0] crc_in;
        input [7:0] data;
        reg [31:0] crc;
        integer i;
        begin
            crc = crc_in ^ {data, 24'h0};
            for (i = 0; i < 8; i = i + 1) begin
                if (crc[31])
                    crc = (crc << 1) ^ 32'h04C11DB7;
                else
                    crc = crc << 1;
            end
            crc32_byte = crc;
        end
    endfunction
endmodule

