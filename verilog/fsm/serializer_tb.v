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

        // Reset phase
        #20;
        rst = 0;

        // Test 1: Send a valid data sequence (First 4 data)
        #20 din = 8'hA1; din_valid = 1; // First data
        #10 din = 8'hB2; din_valid = 1; // Second data
        #10 din = 8'hC3; din_valid = 1; // Third data
        #10 din = 8'hD4; din_valid = 1; // Fourth data
        #10 din_valid = 0; // Stop sending data

        // Wait for serializer to process
        #200;

        // Test 2: Send 16-channel data
        repeat (16) begin
            #10 din = $random & 8'hFF; din_valid = 1; // Use random 8-bit data
        end
        #10 din_valid = 0;

        // Wait for full sequence to complete
        #200;
        
        // End simulation
        $stop;
    end

endmodule
