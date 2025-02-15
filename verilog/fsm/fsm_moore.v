module fsm_counter (
    input wire clk,        // Clock signal
    input wire rst,        // Reset signal
    input wire go,         // Start signal
    output reg done        // Done flag (indicates completion)
);
    
    // State encoding
    typedef enum logic [1:0] {
        IDLE     = 2'b00,  // Waiting state
        COUNTING = 2'b01,  // Counting state
        DONE     = 2'b10   // Completion state
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
                    next_state = DONE; // Transition to DONE when count reaches 15
                else
                    next_state = COUNTING;
            end
            
            DONE: begin
                next_state = DONE; // Stay in DONE state
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

    // Output logic (done signal)
    always @(posedge clk or posedge rst) begin
        if (rst)
            done <= 1'b0;
        else if (state == DONE)
            done <= 1'b1;
        else
            done <= 1'b0;
    end

endmodule
