# report_cdc lab. Run from book/code/ch17 in a terminal:
#   vivado -mode batch -source cdc_lab.tcl
# then open cdc.rpt and clock_interaction.rpt in a text editor.
read_verilog {cdc_lab_top.v sync_2ff.v pulse_sync.v reset_sync.v}
read_xdc cdc_lab.xdc
synth_design -top cdc_lab_top -part xc7a200tsbg484-1
report_cdc -details -file cdc.rpt
report_clock_interaction -file clock_interaction.rpt
