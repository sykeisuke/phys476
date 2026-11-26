module TIMER_TB;

  reg TRG_ONE;
  reg MODE;
  reg CLK;
  reg R;
  wire OUT;

  TIMER uut (
    .TRG_ONE(TRG_ONE),
    .MODE(MODE),
    .CLK(CLK),
    .R(R),
    .OUT(OUT)
  );

  // Clock Generation
  always #5 CLK = ~CLK;  // 10 ns period (100 MHz)

  initial begin
    // Initialize signals
    CLK = 0;
    TRG_ONE = 0;
    MODE = 0;
    R = 0;

    // Reset the system
    #10 R = 1;
    #10 R = 0;

    // Start counting
    #10 TRG_ONE = 1;
    #10 TRG_ONE = 0;  // Set OUT = 1, counter starts

    // Let the counter run until it reaches N
    #3000;

    // Apply MODE
    #10 MODE = 1;
    #10 TRG_ONE = 1;
    #10 TRG_ONE = 0;  // Reset counter when TRG_ONE is received in MODE=1

    // Let it count again
    #3000;

    // End simulation
    #100 $finish;
  end
endmodule
