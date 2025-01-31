`timescale 1ns / 1ps
module RSFF_tb;

    reg S, R;
    wire Q, Qbar;

    RSFF uut (
        .S(S),
        .R(R),
        .Q(Q),
        .Qbar(Qbar)
    );

    initial begin
        
        S = 0; R = 0; #10; 
        S = 1; R = 0; #10; 
        S = 0; R = 1; #10; 
        S = 1; R = 1; #10; 
        S = 0; R = 0; #10; 

        $finish;
    end

endmodule
