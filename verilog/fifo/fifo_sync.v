module fifo_sync (
    input clk,
    input reset,

    input wr_en,
    input [7:0] data_in,     
    output full,

    input rd_en,
    output reg [7:0] data_out,     
    output empty                
);

    parameter FIFO_DEPTH = 8;
    reg [7:0] fifo_mem [0:FIFO_DEPTH-1]; 
    reg [2:0] wr_ptr = 0;             
    reg [2:0] rd_ptr = 0;               
    reg [3:0] count = 0;   

    assign full = (count == FIFO_DEPTH)? 1 : 0;            
    assign empty = (count == 0)? 1 : 0;            

    wire wr_fire = wr_en && !full;
    wire rd_fire = rd_en && !empty;

    // write process
    always @(posedge clk or posedge reset) begin
        if (reset) begin
            wr_ptr <= 0;
        end else if (wr_fire) begin
            fifo_mem[wr_ptr] <= data_in;
            wr_ptr <= (wr_ptr == DEPTH-1) ? 0 : (wr_ptr + 1);
        end
    end

    // read process
    always @(posedge clk or posedge reset) begin
        if (reset) begin
            rd_ptr <= 0;
            data_out <= 0;
        end else if (rd_fire) begin
            data_out <= fifo_mem[rd_ptr]; 
            rd_ptr <= (rd_ptr == DEPTH-1) ? 0 : (rd_ptr + 1);
        end
    end

    // count 
    always @(posedge clk or posedge reset) begin
        if (reset) begin
            count <= 0;
        end else begin
	    case ({wr_fire, rd_fire})
		2'b10: count <= count + 1;
		2'b01: count <= count - 1;
		default: count <= count;
	    endcase
        end
    end

endmodule

