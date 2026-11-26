## Nexys Video constraints for sipo_board.
## Pins from the Digilent Nexys Video master XDC.
set_property -dict {PACKAGE_PIN R4 IOSTANDARD LVCMOS33} [get_ports clk]
create_clock -name sys_clk -period 10.000 [get_ports clk]

set_property -dict {PACKAGE_PIN G4  IOSTANDARD LVCMOS15} [get_ports cpu_resetn]
set_property -dict {PACKAGE_PIN E22 IOSTANDARD LVCMOS12} [get_ports sw]

set_property -dict {PACKAGE_PIN T14 IOSTANDARD LVCMOS25} [get_ports {led[0]}]
set_property -dict {PACKAGE_PIN T15 IOSTANDARD LVCMOS25} [get_ports {led[1]}]
set_property -dict {PACKAGE_PIN T16 IOSTANDARD LVCMOS25} [get_ports {led[2]}]
set_property -dict {PACKAGE_PIN U16 IOSTANDARD LVCMOS25} [get_ports {led[3]}]
