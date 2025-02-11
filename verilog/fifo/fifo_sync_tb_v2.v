`timescale 1ns / 1ps

`define clk_period 10

module fifo_sync_tb_v2();

    reg clk_100MHz;
    reg reset_rtl_0;

    reg wr_en_0;
    reg [17:0] din_0;
    wire full_0;
     
    reg rd_en_0;
    wire [17:0] dout_0;
    wire empty_0;

    // Instantiate the Top Module
    top_module uut (
        .clk_100MHz(clk_100MHz),
        .reset_rtl_0(reset_rtl_0),

        .wr_en_0(wr_en_0),
        .din_0(din_0),
        .full_0(full_0),

        .rd_en_0(rd_en_0),
        .dout_0(dout_0),
        .empty_0(empty_0)                
    );

    integer i;
    initial clk_100MHz = 1'b1;
    always #(`clk_period/2) clk_100MHz = ~clk_100MHz;

    initial begin
        reset_rtl_0 = 1'b1;
        wr_en_0 = 1'b0;
        rd_en_0 = 1'b0;
        din_0 = 18'b0;

        #(`clk_period);
        reset_rtl_0 = 1'b0;

        // Random access test
        for (i = 0; i < 5000; i = i + 1) begin
            wr_en_0 = $random % 2;
            rd_en_0 = $random % 2;
            if (wr_en_0 && !full_0) begin
                din_0 = $random % 262144;  // 18-bit random value
            end

            if (rd_en_0 && !empty_0) begin
            end

            #(`clk_period);
        end

        #(`clk_period);
        #(`clk_period);
        #(`clk_period);

        $stop;
    end

endmodule

