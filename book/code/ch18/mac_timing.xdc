## Clock constraint for the optional WNS lab (mac_comb / mac_pipe).
## 4 ns = 250 MHz: tight on purpose. Only the clock is constrained.
create_clock -period 4.000 -name clk [get_ports clk]
