## Constraints for led_and on the Nexys Video (xc7a200tsbg484-1).
## Lines copied from Digilent's Nexys-Video-Master.xdc
## (github.com/Digilent/digilent-xdc); port names match led_and.v.

## Clock: 100 MHz on pin R4. Uncomment when the top module has a clk port.
#set_property -dict { PACKAGE_PIN R4    IOSTANDARD LVCMOS33 } [get_ports { clk }];
#create_clock -add -name sys_clk_pin -period 10.00 -waveform {0 5} [get_ports clk];

## LEDs (bank voltage 2.5 V)
set_property -dict { PACKAGE_PIN T14   IOSTANDARD LVCMOS25 } [get_ports { led[0] }];
set_property -dict { PACKAGE_PIN T15   IOSTANDARD LVCMOS25 } [get_ports { led[1] }];
set_property -dict { PACKAGE_PIN T16   IOSTANDARD LVCMOS25 } [get_ports { led[2] }];
set_property -dict { PACKAGE_PIN U16   IOSTANDARD LVCMOS25 } [get_ports { led[3] }];
set_property -dict { PACKAGE_PIN V15   IOSTANDARD LVCMOS25 } [get_ports { led[4] }];
set_property -dict { PACKAGE_PIN W16   IOSTANDARD LVCMOS25 } [get_ports { led[5] }];
set_property -dict { PACKAGE_PIN W15   IOSTANDARD LVCMOS25 } [get_ports { led[6] }];
set_property -dict { PACKAGE_PIN Y13   IOSTANDARD LVCMOS25 } [get_ports { led[7] }];

## Switches (bank voltage 1.2 V)
set_property -dict { PACKAGE_PIN E22   IOSTANDARD LVCMOS12 } [get_ports { sw[0] }];
set_property -dict { PACKAGE_PIN F21   IOSTANDARD LVCMOS12 } [get_ports { sw[1] }];
set_property -dict { PACKAGE_PIN G21   IOSTANDARD LVCMOS12 } [get_ports { sw[2] }];
set_property -dict { PACKAGE_PIN G22   IOSTANDARD LVCMOS12 } [get_ports { sw[3] }];
set_property -dict { PACKAGE_PIN H17   IOSTANDARD LVCMOS12 } [get_ports { sw[4] }];
set_property -dict { PACKAGE_PIN J16   IOSTANDARD LVCMOS12 } [get_ports { sw[5] }];
set_property -dict { PACKAGE_PIN K13   IOSTANDARD LVCMOS12 } [get_ports { sw[6] }];
set_property -dict { PACKAGE_PIN M17   IOSTANDARD LVCMOS12 } [get_ports { sw[7] }];
