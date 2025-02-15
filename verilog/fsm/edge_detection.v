module edge_detector_fsm (
    input wire clk,
    input wire rst,
    input wire sig,   
    output reg edge_detected
);

    // State encoding
    parameter LOW  = 1'b0, 
              HIGH = 1'b1;
    
    reg state, next_state;

    // State register (sequential logic)
    always @(posedge clk or posedge rst) begin
        if (rst) begin
            state <= LOW;
        end else begin
            state <= next_state;
        end
    end

    // Next state logic (combinational logic)
    always @(*) begin
        next_state = state; 
        case (state)
            LOW: begin
                if (sig) begin
                    next_state = HIGH;  // 0 → 1 
                end
            end
            HIGH: begin
                if (!sig) begin
                    next_state = LOW;   // 1 → 0
                end
            end
        endcase
    end

    // Output logic: Rising Edge Detection
    always @(posedge clk or posedge rst) begin
        if (rst) begin
            edge_detected <= 1'b0;
        end else if (state == LOW && next_state == HIGH) begin
            edge_detected <= 1'b1; 
        end else begin
            edge_detected <= 1'b0;
        end
    end

endmodule
