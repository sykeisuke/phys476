`timescale 1ns / 1ps

`define clk_period 10

module sync_fifo_tb();

    reg clk;
    reg reset;

    reg wr_en;
    reg [7:0] data_in;
    wire full;
     
    reg rd_en;
    wire [7:0] data_out;
    wire empty;

  fifo_sync uut(
    .clk(clk),
    .reset(reset),

    .wr_en(wr_en),
    .data_in(data_in),     
    .full(full),

    .rd_en(rd_en),
    .data_out(data_out),     
    .empty(empty)                
);

  integer i;
  initial clk = 1'b1;
  always #(`clk_period/2) clk = ~clk;

  initial begin
  	reset = 1'b1;
    	wr_en = 1'b0;
    	rd_en = 1'b0;
	data_in = 8'b0;

	#(`clk_period);
	reset = 1'b0;

        // write data
	wr_en = 1'b1;
	rd_en = 1'b0;

	for (i = 0; i<8; i= i+1) begin
		data_in = i;
		#(`clk_period);
	end

        // read data
	wr_en = 1'b0;
	rd_en = 1'b1;

	for (i = 0; i<8; i= i+1) begin
		#(`clk_period);
	end

	#(`clk_period);
	#(`clk_period);

        // random access
	for (i = 0; i<50; i= i+1) begin
		wr_en = $random %2;
		rd_en = $random %2;
		if (wr_en && !full) begin
			data_in = $random % 256;
		end

		if (rd_en && !empty) begin
		end

		#(`clk_period);
	end


	#(`clk_period);
	#(`clk_period);
	#(`clk_period);

	$stop;
  end

endmodule

