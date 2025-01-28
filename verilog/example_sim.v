module example_sim (
    input SW0,
    input SW1,
    output LED0,
    output LED1,
    output LED2
);

    assign LED0 = SW0 & SW1;   // AND gate
    assign LED1 = SW0 | SW1;   // OR gate
    assign LED2 = SW0 ^ SW1;   // XOR gate

endmodule
