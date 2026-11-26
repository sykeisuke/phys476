`timescale 1ns / 1ps
`default_nettype none

// The 2026 stimulus (SW0 up at 100 ns, SW1 up at 400 ns, SW0 down at 600 ns,
// SW1 down at 1000 ns), now with a check after every step.
module tb_example_sim;
    reg  SW0, SW1;
    wire LED0, LED1, LED2;
    integer errors = 0;

    example_sim inst (.SW0(SW0), .SW1(SW1), .LED0(LED0), .LED1(LED1), .LED2(LED2));

    task expect3(input [2:0] want);        // {LED2, LED1, LED0}
        if ({LED2, LED1, LED0} !== want) begin
            $error("t=%0t SW1 SW0=%b%b: LED2..0 = %b, want %b",
                   $time, SW1, SW0, {LED2, LED1, LED0}, want);
            errors = errors + 1;
        end
    endtask

    initial begin
        $dumpfile("example_sim.vcd");      // waveform file for GTKWave
        $dumpvars(0, tb_example_sim);
        SW0 = 1'b0; SW1 = 1'b0;
        #50  expect3(3'b000);
        #50  SW0 = 1'b1;             // t = 100 ns
        #50  expect3(3'b110);        // XOR = 1, OR = 1, AND = 0
        #250 SW1 = 1'b1;             // t = 400 ns
        #50  expect3(3'b011);
        #150 SW0 = 1'b0;             // t = 600 ns
        #50  expect3(3'b110);
        #350 SW1 = 1'b0;             // t = 1000 ns
        #50  expect3(3'b000);
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
