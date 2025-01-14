/* Concatenation with Ranges*/
wire [23:0] C;
assign C[15:0] = B[15:0];
assign C[23:16] = A[7:0];

/* One-Line Concatenation*/
wire [23:0] C;
assign C = {A[7:0], B[15:0]};

/* Concatenation for Multi-Signal Assignment */
wire [15:0] D;
wire [7:0] E;
assign {D[15:0], E[7:0]} = {A[7:0], B[15:0]};


