module example_sim_tb;

    reg SW0;
    reg SW1;
    wire LED0;
    wire LED1;
    wire LED2;

    example_sim inst (
        .SW0(SW0),
        .SW1(SW1),
        .LED0(LED0),
        .LED1(LED1),
        .LED2(LED2)
    );

    initial begin
        SW0 = 1'b0;
        SW1 = 1'b0;

        #100 SW0 = 1'b1;
        #300 SW1 = 1'b1;
        #200 SW0 = 1'b0;
        #400 SW1 = 1'b0;
    end

endmodule
