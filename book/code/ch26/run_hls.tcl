# Vitis HLS batch flow for muladd (Chapter 26).
# Run in this folder:  vitis_hls -f run_hls.tcl
# (newer unified releases: vitis-run --mode hls --tcl run_hls.tcl ; check yours)
open_project -reset muladd_prj
set_top muladd
add_files muladd.c
add_files -tb muladd_tb.c
open_solution -reset solution1
set_part {xc7a200tsbg484-1}
create_clock -period 10 -name default
csim_design
csynth_design
cosim_design
export_design -format ip_catalog
exit
