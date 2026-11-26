`timescale 1ns / 1ps
`default_nettype none

// Board top for the trigger/busy controller on the Nexys Video.
//   BTNC = trigger (synchronized, debounced, edge detected: Chapter 14)
//   BTNU = 'full' (hold it down to pretend the data buffer is full)
//   A readout takes RD_CYCLES clock cycles (1 s) after start_rd.
//   LED0 = busy, LED[3:1] = rejected triggers, LED[7:4] = accepted triggers.
// Needs sync_2ff.v, tick_gen.v, debounce.v, edge_detect.v from book/code/ch14.
module trig_busy_top #(
    parameter DIV       = 100_000,      // 1 kHz debounce tick
    parameter TW        = 17,
    parameter K         = 8,            // 8 ms stable
    parameter RD_CYCLES = 100_000_000,  // 1 s readout
    parameter RW        = 27
) (
    input  wire       clk,
    input  wire       cpu_resetn,
    input  wire       btnc,
    input  wire       btnu,
    output wire [7:0] led
);
    wire rst, c_s, u_s, tick, c_db, u_db, trig;
    sync_2ff u_rs (.clk(clk), .d_async(~cpu_resetn), .q(rst));
    sync_2ff u_cs (.clk(clk), .d_async(btnc), .q(c_s));
    sync_2ff u_us (.clk(clk), .d_async(btnu), .q(u_s));
    tick_gen #(.DIV(DIV), .W(TW)) u_tick (.clk(clk), .rst(rst), .tick(tick));
    debounce #(.K(K)) u_dbc (.clk(clk), .rst(rst), .tick(tick), .in(c_s), .out(c_db));
    debounce #(.K(K)) u_dbu (.clk(clk), .rst(rst), .tick(tick), .in(u_s), .out(u_db));
    edge_detect u_edge (.clk(clk), .in(c_db), .rise(trig));

    // pretend readout: rd_done one cycle, RD_CYCLES after start_rd
    wire        busy, start_rd;
    wire [15:0] n_acc, n_rej;
    reg  [RW-1:0] rd_cnt;
    reg           reading;
    wire          rd_done = reading && (rd_cnt == RD_CYCLES - 1);

    always @(posedge clk) begin
        if (rst) begin
            reading <= 1'b0;
            rd_cnt  <= {RW{1'b0}};
        end else if (start_rd) begin
            reading <= 1'b1;
            rd_cnt  <= {RW{1'b0}};
        end else if (rd_done) begin
            reading <= 1'b0;
        end else if (reading) begin
            rd_cnt  <= rd_cnt + 1'b1;
        end
    end

    trig_busy u_tb (.clk(clk), .rst(rst), .trig(trig), .rd_done(rd_done), .full(u_db),
                    .busy(busy), .start_rd(start_rd), .n_acc(n_acc), .n_rej(n_rej));

    assign led = {n_acc[3:0], n_rej[2:0], busy};
endmodule

`default_nettype wire
