open_hw
connect_hw_server
open_hw_target

current_hw_device                                 [get_hw_devices xc7a35t_0]

# Tell which file to use
set_property PROGRAM.FILE {a35_fnet/a35_fnet.bit}   [get_hw_devices xc7a35t_0]

# Scan devices
refresh_hw_device -update_hw_probes false [lindex [get_hw_devices xc7a35t_0] 0]

# Program device
program_hw_devices                                [get_hw_devices xc7a35t_0]

## Something for memory?

# create_hw_cfgmem -hw_device [lindex [get_hw_devices] 0] -mem_dev [lindex [get_cfgmem_parts {mt25ql128-spi-x1_x2_x4}] 0]

## By repeated flashing:

close_hw
