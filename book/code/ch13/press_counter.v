`timescale 1ns / 1ps
`default_nettype none

// Counts presses of a push button and shows the count on 8 LEDs.
// It has a deliberate weakness (no debouncing) to find with the ILA.
// mark_debug keeps the marked nets through synthesis so that
// "Set Up Debug" can connect them to an ILA.
module press_counter (
    input  wire       clk,          // 100 MHz
    input  wire       cpu_resetn,   // active low
    input  wire       btnc,         // push button, asynchronous
    output wire [7:0] led
);
    // board reset through the reset synchronizer (Chapters 11 and 12)
    wire rst;
    reset_sync u_rst (.clk(clk), .arst_n(cpu_resetn), .rst(rst));

    // two-flip-flop synchronizer for the button (Chapter 14)
    (* ASYNC_REG = "TRUE" *) reg [1:0] btn_meta = 2'b00;
    always @(posedge clk)
        btn_meta <= {btn_meta[0], btnc};

    (* mark_debug = "true" *) wire btn_s = btn_meta[1];
    (* mark_debug = "true" *) reg  btn_d = 1'b0;
    (* mark_debug = "true" *) wire press = btn_s & ~btn_d;   // rising edge
    (* mark_debug = "true" *) reg  [7:0] count = 8'd0;

    always @(posedge clk)
        if (rst) begin
            btn_d <= 1'b0;
            count <= 8'd0;
        end else begin
            btn_d <= btn_s;
            if (press)
                count <= count + 8'd1;
        end

    assign led = count;
endmodule

`default_nettype wire
