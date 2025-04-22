# filename: build.tcl

set projname [lindex $argv 0]
set use_rataser [lindex $argv 1]

set shorttime [clock format [clock seconds] -format {%s}]

puts "$projname $shorttime $use_rataser"

source ../common.tcl

source ../cfg_$projname.tcl

# Assign part to in-memory project (will also create the in-memory project)
# Used when generating ip and executing synth, impl.
set_part $partname

# Read all design files
read_vhdl ../../../vhdl/fnet_records.vhd
read_vhdl ../../../vhdl/fnet_util_pkg.vhd
read_vhdl ../../../vhdl/fnet_crc32.vhd
read_vhdl ../../../vhdl/fnet_in_state.vhd
read_vhdl ../../../vhdl/fnet_local_reg.vhd
read_vhdl ../../../vhdl/fnet_out_pipeline.vhd
read_vhdl ../../../vhdl/fnet_out_state.vhd
read_vhdl ../../../vhdl/fnet_ram_block_a11d16.vhd
read_vhdl ../../../vhdl/fnet_ram_block_data.vhd
read_vhdl ../../../vhdl/fnet_ram_block_stat.vhd
read_vhdl ../../../vhdl/fnet_regacc_aux_stat.vhd
read_vhdl ../../../vhdl/fnet_regaccess.vhd
read_vhdl ../../../vhdl/fnet_tcp_buffer.vhd
read_vhdl ../../../vhdl/fnet_tcp_control.vhd
read_vhdl ../../../vhdl/fnet_tcp_prepare.vhd
read_vhdl ../../../vhdl/fnet_tcp_state.vhd
read_vhdl ../../../vhdl/fnet_dyn_control.vhd
read_vhdl ../../../vhdl/fnet_ntp_query_control.vhd
read_vhdl ../../../vhdl/fnet_mon_pkt_control.vhd
read_vhdl ../../../vhdl/fnet_packet_gen.vhd
read_vhdl ../../../vhdl/fnet_mdio.vhd
read_vhdl ../../../vhdl/fnet_test_datagen.vhd
read_vhdl ../../../vhdl/fakernet_module.vhd
read_vhdl ../../../vhdl/fnet_async_fifo.vhd
read_vhdl ../../../extra_vhdl/efnet_data_array_inject.vhd
read_vhdl ../../../extra_vhdl/efnet_reg_counters.vhd
read_vhdl ../../../extra_vhdl/efnet_rx_speed_sense.vhd
read_vhdl ../../../extra_vhdl/efnet_octet_to_word.vhd
read_vhdl ../../../extra_vhdl/efnet_word_in_fifo.vhd
read_vhdl ../../../extra_vhdl/efnet_word_out_fifo.vhd
read_vhdl ../../../extra_vhdl/efnet_word_to_octet.vhd
read_vhdl ../../../extra_vhdl/efnet_gmii_mii_rx.vhd
read_vhdl ../../../extra_vhdl/efnet_gmii_mii_tx.vhd
read_vhdl ../../../extra_vhdl/efnet_uart_tx.vhd
read_vhdl ../../../extra_vhdl/efnet_uart_rx.vhd
read_vhdl ../../../extra_vhdl/efnet_slip_rx.vhd
read_vhdl ../../../extra_vhdl/efnet_slip_tx.vhd
read_vhdl ../../../extra_vhdl/ddr/efnet_xilinx_iddr_by_iddr2.vhd
read_vhdl ../../../extra_vhdl/ddr/efnet_xilinx_oddr_by_oddr2.vhd
read_vhdl ../../../extra_vhdl/ddr/efnet_hw_iddr_vector.vhd
read_vhdl ../../../extra_vhdl/ddr/efnet_hw_oddr_vector.vhd
read_vhdl ../../common/efb_uart_trace_mem.vhd
read_vhdl ../../common/efb_nmea_parse.vhd
read_vhdl ../../common/efb_ublox_parse.vhd
read_vhdl ../../common/efb_track_pps_ts.vhd
read_vhdl ../../common/efb_count_low_bits.vhd
read_vhdl ../../common/efb_xilinx_dna_port.vhd
read_vhdl ../../common/efb_xilinx_xadc.vhd
read_vhdl ../../common/efb_spi_flash_read.vhd
read_vhdl ../../common/efb_spi_read_mux.vhd
read_vhdl ../../common/efb_spi_text_config.vhd
read_vhdl ../../common/efb_text_config_parse.vhd
read_vhdl ../../common/efb_lmd_format_events.vhd
read_vhdl ../../common/efb_lmd_buffer_events.vhd
read_vhdl ../../common/efb_sim_events.vhd
read_vhdl ../../common/efb_pmod_gps.vhd
read_vhdl ../../common/rataser_4phase_sample_iserdes2.vhd
read_vhdl ../../common/efb_sample_x16.vhd
read_vhdl ../../common/efb_sampler.vhd
read_vhdl ../../common/efb_pulser.vhd
read_vhdl ../../common/efb_xilinx_common_clk.vhd

if { $use_rataser } {
    read_vhdl "../../../../rataser/vhdl/rataser_records.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_util.vhd"
    #read_vhdl "../../../../rataser/vhdl/generic/rataser_4phase_sample.vhd"
    read_vhdl "../../../../rataser/vhdl/xilinx/rataser_4phase_sample_iserdes2.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_4phase_deser.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_big_xor.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_hamming_util.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_hamming_encode.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_hamming_decode.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_hamming_decode_dual.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_sync_count.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_min_max_filter.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_track_bitstream.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_track_edge.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_sym8word1_decode.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_sym24word8_util.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_sym24word8_diff.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_sym24word8_decode.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_timestamp_decode.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_clock_send.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_clock_recv.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_signals_send.vhd"
    read_vhdl "../../../../rataser/vhdl/rataser_signals_recv.vhd"
} else {
    read_vhdl "../../common/rataser_dummy_clock_send.vhd"
    read_vhdl "../../common/rataser_dummy_clock_recv.vhd"
}

read_vhdl ../../common/efb_common_top.vhd
read_vhdl ../$toplevel.vhd

# Generated text config setup
read_vhdl text_config_data.vhd

# Read constraints
read_xdc ../$xdcfile
if { [ info exists xdcfile2 ] } {
    read_xdc ../$xdcfile2
}

# Generate
generate_target all [get_ips]

# Synthesize
synth_design -top $toplevel -part $partname \
    -generic compiletime=$shorttime {*}$generics
# Note: the {*} breaks the variable up into multiple arguments.

report_timing_summary -file timing.synth.out -delay_type min_max -max_path 10

# Optimise
opt_design

# Place
place_design

# Route
route_design

write_edif -force arty.edif

write_checkpoint -force arty.dcp

report_timing_summary -file timing.out -delay_type min_max -max_path 10

report_utilization

# Write out bitfile
write_debug_probes -force $projname.ltx
write_bitstream    -force $projname.bit -bin_file
