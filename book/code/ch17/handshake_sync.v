`timescale 1ns / 1ps
`default_nettype none

// Bus crossing with a four-phase req/ack handshake.
// Source: on 'send' (when !busy) latch data, raise req. Hold data while req=1.
// Destination: sees req (synchronized), captures data, raises ack.
// Source drops req when it sees ack; destination drops ack when req falls.
module handshake_sync #(parameter W = 8) (
    input  wire         clk_a,
    input  wire         send,       // one-cycle request in clk_a
    input  wire [W-1:0] data_a,
    output wire         busy,       // 1 while a transfer is in flight
    input  wire         clk_b,
    output reg  [W-1:0] data_b = {W{1'b0}},
    output reg          valid_b = 1'b0  // one-cycle pulse in clk_b
);
    // ---- source domain
    reg         req = 1'b0;
    reg [W-1:0] hold = {W{1'b0}};
    wire        ack_a;
    always @(posedge clk_a) begin
        if (send && !busy) begin
            hold <= data_a;
            req  <= 1'b1;
        end else if (ack_a) begin
            req  <= 1'b0;
        end
    end
    assign busy = req | ack_a;

    // ---- destination domain
    wire req_b;
    reg  ack = 1'b0;
    sync_2ff u_req (.clk(clk_b), .d(req), .q(req_b));
    always @(posedge clk_b) begin
        valid_b <= 1'b0;
        if (req_b && !ack) begin
            data_b  <= hold;         // stable: req has been high for 2+ cycles
            valid_b <= 1'b1;
            ack     <= 1'b1;
        end else if (!req_b) begin
            ack     <= 1'b0;
        end
    end
    sync_2ff u_ack (.clk(clk_a), .d(ack), .q(ack_a));
endmodule

`default_nettype wire
