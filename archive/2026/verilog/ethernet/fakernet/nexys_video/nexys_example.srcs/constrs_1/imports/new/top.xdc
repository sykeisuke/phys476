set_property SEVERITY {Warning} [get_drc_checks NSTD-1]
set_property SEVERITY {Warning} [get_drc_checks UCIO-1]

# Clock
set_property -dict { PACKAGE_PIN R4 IOSTANDARD LVCMOS33 } [get_ports { clk }]
create_clock -add -name sys_clk_pin -period 10.00 -waveform {0 5} [get_ports { clk }]

# MDIO interface
set_property -dict { PACKAGE_PIN AA16 IOSTANDARD LVCMOS25 } [get_ports { phy_mdc }]
set_property -dict { PACKAGE_PIN Y16 IOSTANDARD LVCMOS25 } [get_ports { phy_mdio }]

# RGMII Receive
set_property -dict { PACKAGE_PIN V13 IOSTANDARD LVCMOS25 } [get_ports { phy_rx_clk }]
set_property -dict { PACKAGE_PIN W10 IOSTANDARD LVCMOS25 } [get_ports { phy_rx_ctl }]
set_property -dict { PACKAGE_PIN AB16 IOSTANDARD LVCMOS25 } [get_ports { phy_rxd[0] }]
set_property -dict { PACKAGE_PIN AA15 IOSTANDARD LVCMOS25 } [get_ports { phy_rxd[1] }]
set_property -dict { PACKAGE_PIN AB15 IOSTANDARD LVCMOS25 } [get_ports { phy_rxd[2] }]
set_property -dict { PACKAGE_PIN AB11 IOSTANDARD LVCMOS25 } [get_ports { phy_rxd[3] }]

# RGMII Transmit
set_property -dict { PACKAGE_PIN AA14 IOSTANDARD LVCMOS25 } [get_ports { phy_tx_clk }]
set_property -dict { PACKAGE_PIN V10  IOSTANDARD LVCMOS25 } [get_ports { phy_tx_ctl }]
set_property -dict { PACKAGE_PIN Y12  IOSTANDARD LVCMOS25 } [get_ports { phy_txd[0] }]
set_property -dict { PACKAGE_PIN W12  IOSTANDARD LVCMOS25 } [get_ports { phy_txd[1] }]
set_property -dict { PACKAGE_PIN W11  IOSTANDARD LVCMOS25 } [get_ports { phy_txd[2] }]
set_property -dict { PACKAGE_PIN Y11  IOSTANDARD LVCMOS25 } [get_ports { phy_txd[3] }]

# Reset and Status
set_property -dict { PACKAGE_PIN U7   IOSTANDARD LVCMOS33 } [get_ports { phy_reset_n }]
set_property -dict { PACKAGE_PIN Y14  IOSTANDARD LVCMOS25 } [get_ports { phy_int_n }]

