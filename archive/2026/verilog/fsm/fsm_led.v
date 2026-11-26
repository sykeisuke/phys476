module fsm_led (
    input wire clk,
    input wire rst,
    output reg LED0,
    output reg LED1,
    output reg LED2
);

  reg [31:0] clk_div_counter = 0;
  reg slow_clk = 0;

  // Clock divider to generate a slower clock signal
  always @(posedge clk) begin
    if (rst) begin
      clk_div_counter <= 0;
      slow_clk <= 0;
    end else begin
      clk_div_counter <= clk_div_counter + 1;
      if (clk_div_counter == 27'd50_000_000) begin
        slow_clk <= ~slow_clk;
        clk_div_counter <= 0;
      end
    end
  end

  // State encoding using parameter
  parameter S0 = 2'b00, S1 = 2'b01, S2 = 2'b10, S3 = 2'b11;
  reg [1:0] state, next_state;

  // State register (sequential logic)
  always @(posedge slow_clk or posedge rst) begin
    if (rst) begin
      state <= S0;
    end else begin
      state <= next_state;
    end
  end

  // Next state and output logic (combinational logic)
  always @(*) begin
    case (state)
      S0: begin 
        {LED2, LED1, LED0} = 3'b001; 
        next_state = S1; 
      end
      S1: begin 
        {LED2, LED1, LED0} = 3'b010; 
        next_state = S2; 
      end
      S2: begin 
        {LED2, LED1, LED0} = 3'b100; 
        next_state = S3; 
      end
      S3: begin 
        {LED2, LED1, LED0} = 3'b000; 
        next_state = S0; 
      end
      default: begin 
        {LED2, LED1, LED0} = 3'b000; 
        next_state = S0; 
      end
    endcase
  end

endmodule
