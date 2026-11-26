`timescale 1ns / 1ps
`default_nettype none

// Self-checking testbench for ip_checksum and crc32_eth.
module tb_ch21;
    integer errors = 0, i;

    // --- IPv4 header checksum -------------------------------------------
    reg  [159:0] hdr;
    wire [15:0]  csum;
    ip_checksum u_ip (.hdr(hdr), .csum(csum));

    // --- CRC-32 -----------------------------------------------------------
    reg         clk = 1'b0, init = 1'b0, en = 1'b0;
    reg  [7:0]  data = 8'd0;
    wire [31:0] fcs;
    crc32_eth u_crc (.clk(clk), .init(init), .en(en), .data(data), .fcs(fcs));
    always #4 clk = ~clk;                     // 125 MHz

    reg [8*9-1:0] msg = "123456789";

    initial begin
        // Example header: 45 00 00 73 00 00 40 00 40 11 [b8 61] c0 a8 00 01 c0 a8 00 c7
        hdr = 160'h4500_0073_0000_4000_4011_0000_c0a8_0001_c0a8_00c7;
        #1 if (csum !== 16'hb861) begin
            $display("FAIL: checksum %h, expected b861", csum); errors = errors + 1;
        end
        hdr[79:64] = 16'hb861;                // received header with checksum
        #1 if (csum !== 16'h0000) begin
            $display("FAIL: check of a good header gives %h", csum); errors = errors + 1;
        end
        // Hello World header (from build_udp_frame.py): total length 39
        hdr = 160'h4500_0027_0000_4000_4011_0000_c0a8_0132_c0a8_0164;
        #1 if (csum !== 16'hb6df) begin
            $display("FAIL: hello checksum %h, expected b6df", csum); errors = errors + 1;
        end

        // CRC-32 check value of "123456789" is cbf43926
        @(negedge clk) init = 1'b1;
        @(negedge clk) init = 1'b0;
        for (i = 8; i >= 0; i = i - 1) begin
            en = 1'b1; data = msg[8*i +: 8];
            @(negedge clk);
        end
        en = 1'b0;
        if (fcs !== 32'hcbf43926) begin
            $display("FAIL: crc %h, expected cbf43926", fcs); errors = errors + 1;
        end

        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
