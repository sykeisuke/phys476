# Fakernet on Nexys Video

## Overview

This repository provides an implementation of **Fakernet** on the **Nexys Video** FPGA board (Artix-7), using the **Realtek RTL8211E-VL** PHY for 1 Gbps Ethernet communication.

For general information about Fakernet, please visit:  
👉 https://fy.chalmers.se/~f96hajo/fakernet/

The control software (`fnetctrl`) can also be downloaded from the above page.

---

## IP Configuration

The default IP address is fixed at `192.168.1.192`, as defined in the VHDL source:

```vhdl
-- c0a801c0 = 192.168.1.192
signal ipaddr  : std_logic_vector(31 downto 0) :=
  "11000000" & "10101000" & "00000001" & "11000000";
```

You may change this value or connect it to DIP switches for dynamic configuration.

## Clock Requirement

Connect a `free-running 125 MHz clock` to the clk_in port.
This clock is required for 1G Ethernet operation and must also be connected to the data generator module if used.

## Testing

After compiling the control program (see client/ directory in the original Fakernet repo), configure your PC’s IPv4 address (e.g. 192.168.1.100) and subnet mask (255.255.255.0). Then connect to the FPGA via Ethernet.

### 1. Quick Test
```bash
ping 192.168.1.192
./fnetctrl 192.168.1.192 --stat
```

### 2. TCP Throughput Test (Internal Data Generator)
```bash
./fnetctrl 192.168.1.192 --tcp-lcl-datagen=1
./fnetctrl 192.168.1.192 --tcp-lcl-data-chance=100000000
./fnetctrl 192.168.1.192 --tcp-lcl-data-valmask=8191
./fnetctrl 192.168.1.192 --tcp=local
```
Expected throughput (1G): ~117,000 kB/s.

### 3. TCP with Custom User Data Generator
```bash
./fnetctrl 192.168.1.192 --write=0x05:3
./fnetctrl 192.168.1.192 --tcp-lcl-datagen=0
./fnetctrl 192.168.1.192 --tcp
```
Expected throughput (1G): ~27,300 kB/s.
See the file `data_gen_user.vhd` for details of the generated data format.

## License
This implementation is based on Fakernet (BSD 3-Clause License). See original license for details.


