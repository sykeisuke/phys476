`timescale 1ns / 1ps
`default_nettype none

// Edge detection written as a two-state FSM (LOW, HIGH). 'rise' is a
// registered one-cycle pulse when the state goes from LOW to HIGH.
// 'in' must already be synchronized and debounced (Chapter 14).
module edge_fsm (
    input  wire clk,
    input  wire rst,
    input  wire in,
    output reg  rise
);
    localparam LOW = 1'b0, HIGH = 1'b1;

    reg state, next;

    always @(*) begin
        next = state;
        case (state)
            LOW:     if (in)  next = HIGH;
            HIGH:    if (!in) next = LOW;
            default:          next = LOW;
        endcase
    end

    always @(posedge clk) begin
        if (rst) begin
            state <= LOW;
            rise  <= 1'b0;
        end else begin
            state <= next;
            rise  <= (state == LOW) && (next == HIGH);
        end
    end
endmodule

`default_nettype wire
