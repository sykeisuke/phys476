`timescale 1ns / 1ps
`default_nettype none

// A small FSM that empties a standard FIFO (dout one clock after rd_en)
// into an output port. IDLE: if the FIFO has data and the receiver is
// ready, pulse rd_en. WAIT: the word appears on dout; pass it on.
module fifo_drain #(
    parameter WIDTH = 8
) (
    input  wire             clk,
    input  wire             rst,
    // FIFO read side
    input  wire             empty,
    input  wire [WIDTH-1:0] dout,
    output reg              rd_en,
    // output port
    input  wire             out_ready,
    output reg              out_valid,
    output reg  [WIDTH-1:0] out_data
);
    localparam IDLE = 1'b0, WAIT = 1'b1;
    reg state, next;

    always @(*) begin
        next  = state;                   // defaults first
        rd_en = 1'b0;
        case (state)
            IDLE: if (!empty && out_ready) begin
                      rd_en = 1'b1;
                      next  = WAIT;
                  end
            WAIT: next = IDLE;
            default: next = IDLE;
        endcase
    end

    always @(posedge clk) begin
        if (rst) begin
            state     <= IDLE;
            out_valid <= 1'b0;
            out_data  <= {WIDTH{1'b0}};
        end else begin
            state     <= next;
            out_valid <= (state == WAIT);    // word is on dout now
            if (state == WAIT)
                out_data <= dout;
        end
    end
endmodule

`default_nettype wire
