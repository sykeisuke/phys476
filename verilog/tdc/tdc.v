module fine_tdc (
    input wire clk,
    input wire rst,
    input wire signal_in,
    output reg [15:0] coarse_time,
    output reg [7:0] fine_time
);

    reg [15:0] counter;
    reg [7:0] delay_line;

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            counter <= 0;
            coarse_time <= 0;
            fine_time <= 0;
        end else begin
            counter <= counter + 1;
            if (signal_in) begin
                coarse_time <= counter;
                fine_time <= delay_line;
            end
        end
    end

    always @(signal_in) begin
        delay_line <= {delay_line[6:0], signal_in};
    end

endmodule

