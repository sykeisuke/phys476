always @(posedge CLK) begin
    A <= B;  
    B <= C;  
end

always @(*) begin
    A = B;  
    B = C;  
end
