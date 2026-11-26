`timescale 1ns / 1ps

module tff_chain_tb;

    reg CLK;
    wire Q1, Q2, Q3;

    // Instantiate the DUT (Device Under Test)
    tff_chain uut (
        .CLK(CLK),
        .Q1(Q1),
        .Q2(Q2),
        .Q3(Q3)
    );

    // Clock generation
    always #5 CLK = ~CLK; // 10 ns period (100 MHz)

    initial begin
        // Initialize
        CLK = 0;

        // Run for some time
        #100;
        
        // End simulation
        $finish;
    end

endmodule
