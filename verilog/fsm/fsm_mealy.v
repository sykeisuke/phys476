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
    always @(posedge clk or posedge rst) begin
        if (rst)
            state <= IDLE; // Reset to IDLE
        else
            state <= next_state;
    end

    // Next state logic (combinational logic)
    always @(*) begin
        case (state)
            IDLE: begin
                if (go)
                    next_state = COUNTING; // Start counting
                else
                    next_state = IDLE;
            end
            
            COUNTING: begin
                if (count == 4'hF)
                    next_state = IDLE; // Reset after completion
                else
                    next_state = COUNTING;
            end
            
            default: next_state = IDLE;
        endcase
    end

    // Counter logic
    always @(posedge clk or posedge rst) begin
        if (rst)
            count <= 4'b0000;
        else if (state == COUNTING)
            count <= count + 1'b1;
    end

    // Mealy Output Logic (depends on state and input)
    always @(*) begin
        if (state == COUNTING && count == 4'hF)
            done = 1'b1; // Immediate output change when count reaches 15
        else
            done = 1'b0;
    end

endmodule
