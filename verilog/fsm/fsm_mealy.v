module fsm_counter_mealy (
    input wire clk,        
    input wire rst,        
    input wire go,         
    output reg done        
);
    
    // State encoding
    typedef enum logic [1:0] {
        IDLE     = 2'b00,  
        COUNTING = 2'b01   
    } state_t;
    
    state_t state, next_state;
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
                    next_state = IDLE; // Reset after completion
                end else begin
                    next_state = COUNTING;
                end
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

    // Mealy Output Logic (depends on state and input)
    always @(*) begin
        if (state == COUNTING && count == 4'hF) begin
            done = 1'b1; // Immediate output change when count reaches 15
        end else begin
            done = 1'b0;
        end
    end

endmodule

