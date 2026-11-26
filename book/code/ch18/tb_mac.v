`timescale 1ns / 1ps
`default_nettype none

// Compares mac_comb and mac_pipe with a reference model and
// measures their latency (cycles from in_valid to out_valid).
module tb_mac;
    reg         clk = 1'b0;
    reg         in_valid = 1'b0;
    reg  [7:0]  a = 8'd0, b = 8'd0;
    reg  [15:0] c = 16'd0;
    wire        vc, vp;
    wire [16:0] yc, yp;
    integer     errors = 0, cyc = 0, t_in = -1, lat_c = -1, lat_p = -1;
    integer     n_c = 0, n_p = 0, i;
    reg  [16:0] ref_q [0:63];

    always #5 clk = ~clk;
    always @(posedge clk) cyc <= cyc + 1;

    mac_comb u_c (.clk(clk), .in_valid(in_valid), .a(a), .b(b), .c(c),
                  .out_valid(vc), .y(yc));
    mac_pipe u_p (.clk(clk), .in_valid(in_valid), .a(a), .b(b), .c(c),
                  .out_valid(vp), .y(yp));

    // check outputs against the reference queue
    always @(posedge clk) begin
        if (vc) begin
            if (lat_c < 0) lat_c = cyc - t_in;
            if (yc !== ref_q[n_c]) begin
                $display("FAIL comb #%0d: %0d != %0d", n_c, yc, ref_q[n_c]);
                errors = errors + 1;
            end
            n_c = n_c + 1;
        end
        if (vp) begin
            if (lat_p < 0) lat_p = cyc - t_in;
            if (yp !== ref_q[n_p]) begin
                $display("FAIL pipe #%0d: %0d != %0d", n_p, yp, ref_q[n_p]);
                errors = errors + 1;
            end
            n_p = n_p + 1;
        end
    end

    initial begin
        repeat (3) @(posedge clk);
        for (i = 0; i < 64; i = i + 1) begin
            @(negedge clk);
            a = $random;
            b = $random;
            c = $random;
            if (i == 0) a = 8'd255;
            if (i == 0) b = 8'd255;
            if (i == 0) c = 16'hFFFF;       // largest result: 17 bits
            ref_q[i] = a * b + c;
            in_valid = 1'b1;
            if (i == 0) t_in = cyc;         // posedge that samples word 0
        end
        @(negedge clk) in_valid = 1'b0;
        repeat (6) @(posedge clk);
        if (n_c != 64 || n_p != 64) begin
            $display("FAIL: got %0d / %0d results", n_c, n_p);
            errors = errors + 1;
        end
        if (lat_c != 2 || lat_p != 3) begin
            $display("FAIL: latency comb %0d pipe %0d", lat_c, lat_p);
            errors = errors + 1;
        end
        $display("latency: comb %0d cycles, pipe %0d cycles; 64 results each",
                 lat_c, lat_p);
        if (errors == 0) $display("PASS");
        else             $display("FAIL: %0d errors", errors);
        $finish;
    end
endmodule

`default_nettype wire
