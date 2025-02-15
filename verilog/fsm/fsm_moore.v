module fsm_counter (
    input wire clk,        
    input wire rst,        
    input wire go,         
    output reg done        
);
    
    // State encoding
    parameter [1:0] IDLE = 2'b00, COUNTING = 2'b01, DONE = 2'b10;
    reg [1:0] state, next_state;

    reg [3:0] count; // 4-bit counter (0 to 15)

    // State register (sequential logic)
    always @(posedge clk) begin
        if (rst) begin
            state <= IDLE; // Reset to IDLE
        end else begin
            state <= next_state;
        end
    end

    // Next state logic (combinational logic)
    always @(*) begin
        case (state)
            IDLE: begin
                if (go) begin
                    next_state = COUNTING; // Start counting
                end else begin
                    next_state = IDLE;
                end
            end
            
            COUNTING: begin
                if (count == 4'hF) begin
                    next_state = DONE; // Transition to DONE when count reaches 15
                end else begin
                    next_state = COUNTING;
                end
            end
            
            DONE: begin
                next_state = DONE; // Stay in DONE state
            end
            
            default: begin
                next_state = IDLE;
            end
        endcase
    end

    // Counter logic
    always @(posedge clk) begin
        if (rst) begin
            count <= 4'b0000;
        end else if (state == COUNTING) begin
            count <= count + 1'b1;
        end
    end

    // Output logic (done signal)
    always @(posedge clk) begin
        if (rst) begin
            done <= 1'b0;
        end else if (state == DONE) begin
            done <= 1'b1;
        end else begin
            done <= 1'b0;
        end
    end

endmodule

