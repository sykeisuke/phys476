module simple_tdc (
    input wire clk,       
    input wire rst,      
    input wire start,      
    input wire stop,
    output reg [15:0] time_count
);

    reg [15:0] counter;
    reg running;

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            counter <= 0;
            running <= 0;
            time_count <= 0;
        end else begin
            if (start) begin
                counter <= 0;
                running <= 1;
            end
            if (running) begin
                counter <= counter + 1;
            end
            if (stop) begin
                time_count <= counter;
                running <= 0;
            end
        end
    end

endmodule
