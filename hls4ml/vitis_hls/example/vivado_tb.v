`timescale 1ns / 1ps

module muladd_tb;

  parameter SIZE = 16;

  // クロック・リセット
  reg clk;
  reg rst;

  // 制御信号
  reg ap_start;
  wire ap_done;
  wire ap_idle;
  wire ap_ready;

  // 入力メモリ信号
  wire [3:0] a_address0;
  wire a_ce0;
  reg [15:0] a_q0;

  wire [3:0] b_address0;
  wire b_ce0;
  reg [15:0] b_q0;

  // 出力
  wire [31:0] ap_return;

  // DUTインスタンス（IP名が "muladd_0" の場合）
  muladd_0 uut (
    .ap_clk(clk),
    .ap_rst(rst),
    .ap_start(ap_start),
    .ap_done(ap_done),
    .ap_idle(ap_idle),
    .ap_ready(ap_ready),
    .a_address0(a_address0),
    .a_ce0(a_ce0),
    .a_q0(a_q0),
    .b_address0(b_address0),
    .b_ce0(b_ce0),
    .b_q0(b_q0),
    .ap_return(ap_return)
  );

  // 入力データ
  reg [15:0] a_mem [0:SIZE-1];
  reg [15:0] b_mem [0:SIZE-1];
  reg [31:0] expected;
  reg [31:0] result;

  // クロック生成
  always #5 clk = ~clk;

  initial begin
    integer i;
    clk = 0;
    rst = 1;
    ap_start = 0;
    expected = 0;

    // 初期化
    #20;
    rst = 0;

    for (i = 0; i < SIZE; i = i + 1) begin
      a_mem[i] = i;
      b_mem[i] = i + 1;
      expected = expected + a_mem[i] * b_mem[i];
    end

    #20;
    ap_start = 1;
    #10;
    ap_start = 0;
  end

  // メモリ読み出し（a_q0, b_q0）
  always @ (posedge clk) begin
    if (a_ce0)
      a_q0 <= a_mem[a_address0];
    if (b_ce0)
      b_q0 <= b_mem[b_address0];
  end

  // 結果チェック
  always @ (posedge clk) begin
    if (ap_done) begin
      result <= ap_return;
      #10;
      $display("Result    : %h", result);
      $display("Expected  : %h", expected);
      if (result == expected)
        $display("PASS");
      else
        $display("FAIL");
      $stop;
    end
  end

endmodule
