reg [3:0] Q;

always@ (posedge CLK) begin
  Q[0] <= SLI;
end

always@ (posedge CLK) begin
  Q[1] <= Q[0];
end

always@ (posedge CLK) begin
  Q[2] <= Q[1];
end

always@ (posedge CLK) begin
  Q[3] <= Q[2];
end


reg [3:0] Q;

always@ (posedge CLK) begin
  Q[0] <= SLI;
  Q[1] <= Q[0];
  Q[2] <= Q[1];
  Q[3] <= Q[2];
end


