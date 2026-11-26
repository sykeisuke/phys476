# Non-project build of led_and for the Nexys Video.
# Run: vivado -mode batch -source build.tcl
read_verilog led_and.v
read_xdc     nexys_video.xdc
synth_design -top led_and -part xc7a200tsbg484-1
opt_design
place_design
route_design
report_utilization    -file util.rpt
report_timing_summary -file timing.rpt
write_bitstream -force led_and.bit
