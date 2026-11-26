`timescale 1ns / 1ps
`default_nettype none

// Mealy version of fsm_moore (exercise 1): two states, and 'done'
// depends on the state AND the input count. It is 1 in the cycle where
// count reaches 15, one cycle earlier than the Moore output, but it
// comes from combinational logic.
module fsm_mealy (
    input  wire       clk,
    input  wire       rst,
    input  wire       go,
    output reg  [3:0] count,
    output wire       done
);
    localparam IDLE = 1'b0, COUNTING = 1'b1;

    reg state, next;

    assign done = (state == COUNTING) && (count == 4'hF);   // Mealy output

    always @(*) begin
        next = state;
        case (state)
            IDLE:     if (go)   next = COUNTING;
            COUNTING: if (done) next = IDLE;
            default:            next = IDLE;
        endcase
    end

    always @(posedge clk) begin
        if (rst) begin
            state <= IDLE;
            count <= 4'd0;
        end else begin
            state <= next;
            count <= (state == COUNTING) ? count + 4'd1 : 4'd0;
        end
    end
endmodule

`default_nettype wire
