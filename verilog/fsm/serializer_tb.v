`timescale 1ns / 1ps

module serializer_tb;
    
    reg clk;
    reg rst;
    reg [7:0] din;
    wire [7:0] dout;

    // Instantiate the serializer module
    serializer uut (
        .clk(clk),
        .rst(rst),
        .din(din),
        .dout(dout)
    );

    // Clock generation (100 MHz = 10 ns period)
    always #5 clk = ~clk;

    initial begin
        // Initialize signals
        clk = 0;
        rst = 1;
        din = 8'h00;

        // Reset phase
        #20;
        rst = 0;

        // Test 1: Send 4 data samples, then stop (less than NUM_CHANNELS)
        #20 din = 8'hA1;  
        #10 din = 8'hB2;  
        #10 din = 8'hC3;  
        #10 din = 8'hD4;  

        // check if footer is sent out
        #200;

        // Test 2: Send full 16-channel data
        repeat (16) begin
            #10 din = $random & 8'hFF; 
        end

        // Wait for full sequence to complete
        #200;
        
        // End simulation
        $stop;
    end

endmodule
