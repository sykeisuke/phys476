`timescale 1ns / 1ps
`default_nettype none

// Trigger/busy controller (Moore). A trigger in IDLE starts one readout;
// triggers that arrive while busy are counted as rejected. While the
// 'full' input is 1 (the data buffer has no room; buffers come in the
// next chapter) the controller stays busy (HOLD).
// Outputs are registered: they are computed from 'next' and stored on
// the same edge as the state, so they change only on clock edges.
module trig_busy (
    input  wire        clk,
    input  wire        rst,          // synchronous, active high
    input  wire        trig,         // one-cycle pulse, synchronous to clk
    input  wire        rd_done,      // one-cycle pulse: readout finished
    input  wire        full,         // data buffer has no room
    output reg         busy,
    output reg         start_rd,     // one-cycle pulse: start readout
    output reg  [15:0] n_acc,        // accepted triggers
    output reg  [15:0] n_rej         // rejected triggers
);
    localparam [1:0] IDLE = 2'd0, START = 2'd1, READ = 2'd2, HOLD = 2'd3;

    reg [1:0] state, next;

    always @(*) begin
        next = state;
        case (state)
            IDLE:    if (full)        next = HOLD;
                     else if (trig)    next = START;
            START:   next = READ;
            READ:    if (rd_done)      next = full ? HOLD : IDLE;
            HOLD:    if (!full)       next = IDLE;
            default: next = IDLE;
        endcase
    end

    always @(posedge clk) begin
        if (rst) begin
            state    <= IDLE;
            busy     <= 1'b0;
            start_rd <= 1'b0;
            n_acc    <= 16'd0;
            n_rej    <= 16'd0;
        end else begin
            state    <= next;
            busy     <= (next != IDLE);
            start_rd <= (next == START);
            if (trig) begin
                if (state == IDLE && next == START)
                    n_acc <= n_acc + 16'd1;
                else
                    n_rej <= n_rej + 16'd1;
            end
        end
    end
endmodule

`default_nettype wire
