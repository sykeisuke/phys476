module switch_counter(
    input CLK,            
    input SWIN,            
    output wire CA, CB, CC, CD, CE, CF, CG,
    output wire AN0, AN1, AN2, AN3, AN4, AN5, AN6, AN7
);

    reg Q0, Q1, Q2;
    reg CEn;
    reg [3:0] counter;
    
    // 3 DFFs for debounching
    always @(posedge CLK) begin
      Q0 <= SWIN;
      Q1 <= Q0;
      Q2 <= Q1;

      // one-shot pulse
      if (Q1 && ~Q2) begin
          CEn <= 1'b1;
      end else begin
          CEn <= 1'b0;
      end
    end
    
    // counter
    always @(posedge CLK) begin
      if (CEn) begin
        if (counter == 4'd9) begin
          counter <= 4'd0;
        end else begin
          counter <= counter + 1'd1;
        end
      end
    end

    // 7-segment display logic
    assign {CG, CF, CE, CD, CC, CB, CA}
	= (counter == 4'd0) ? 7'b1000000 : // "0"
          (counter == 4'd1) ? 7'b1111001 : // "1"
          (counter == 4'd2) ? 7'b0100100 : // "2"
          (counter == 4'd3) ? 7'b0110000 : // "3"
          (counter == 4'd4) ? 7'b0011001 : // "4"
          (counter == 4'd5) ? 7'b0010010 : // "5"
          (counter == 4'd6) ? 7'b0000010 : // "6"
          (counter == 4'd7) ? 7'b1111000 : // "7"
          (counter == 4'd8) ? 7'b0000000 : // "8"
          (counter == 4'd9) ? 7'b0010000 : // "9"
          7'b1111111; // Default (all segments off)

