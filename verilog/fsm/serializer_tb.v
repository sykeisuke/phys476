`timescale 1ns / 1ps

module serializer_tb;
    
    reg clk;
    reg rst;
    reg [7:0] din;
    reg din_valid;
    wire [7:0] dout;
    wire dout_valid;

    // Instantiate the serializer module
    serializer uut (
        .clk(clk),
        .rst(rst),
        .din(din),
        .din_valid(din_valid),
        .dout(dout),
        .dout_valid(dout_valid)
    );

    // Clock generation (100 MHz = 10 ns period)
    always #5 clk = ~clk;

    initial begin
        // Initialize signals
        clk = 0;
        rst = 1;
        din = 8'h00;
        din_valid = 0;

        #20;
        rst = 0;

        // Test 1: Send 16 data samples
        #10 din_valid = 1;
        for (i = 0; i < 16; i = i + 1) begin
            #10 din = $random; 
        end
        #10 din_valid = 0; 
        #10 din = 8'h00;
        #200; // Wait for full packet transmission

        // Test 2: Another 16 random data samples
        #50 din_valid = 1;
        for (i = 0; i < 16; i = i + 1) begin
            #10 din = $random;
        end
        #10 din_valid = 0;  
        #10 din = 8'h00;
        #200;

        // End simulation
        $stop;
    end

endmodule

