# CHANGELOG – Waveform Transfer via Fakernet (UDP Register Interface)

---

## Data Preparation
- Created `waveform_data_0.bin`
  - Each event contains **100 float values**
  - Stored as a binary file with **32-bit float values**

---

##  Software Modifications
- **New file**: `fnetctrl_sendwaveform.c`  
  - Implements `fnet_send_waveform()` to send waveform data via UDP register interface
- Extended `fnet_client.c` and `fnet_client.h`
  - Added `fnet_ctrl_write_register()` for low-level UDP register access
- Updated **Makefile**
  - Included `fnetctrl_sendwaveform.c` in the build targets
- Updated **`fnetctrl.c`**
  - Added CLI option `--send-waveform`
  - Parses waveform file and sends data to `0x1000–0x1063`
- Added CLI command for **triggering result send-back**:
  ```bash
  ./fnetctrl 192.168.1.192 --noidempotent --write=0x0005:0x3
  ./fnetctrl 192.168.1.192 --tcp=hex
  ```

---

##  RTL / Vivado Modifications

### `fnet_regaccess.vhd`
- Added waveform data path and write-enable logic:
  ```vhdl
  waveform_data_out <= waveform_mem(conv_integer(regacc_pre_addr(5 downto 0)));
  waveform_mem(conv_integer(regacc_pre_addr(5 downto 0))) <= regacc_pre_data_wr;
  waveform_wr_out <= '1' when regacc_pre_ext_write = '1' and 
                     regacc_pre_addr(15 downto 0) in x"1000" to x"1063" else '0';
  regacc_pst2_done <= '1'; -- temporary workaround
  ```

### `fakernet_module.vhd`
- Added new ports:
  - `waveform_data_out : out std_logic_vector(31 downto 0);`
  - `waveform_wr_out   : out std_logic;`
- Connected internally to `fnet_regaccess`

### `efb_common_top.vhd`
- Connected `waveform_data_out` and `waveform_wr_out` to `top.v`
- Passed `user_data_*` signals to top level for result output

### `top.v`
- Instantiated:
  - FIFO (for buffering waveform)
  - `dummy_hls4ml_ip` (simulated HLS module)
  - `hls4ml_wrapper` (handles FIFO-to-IP interfacing)
- Internal logic:
  - FIFO write via `waveform_wr_out`
  - 100-sample batch collection
  - HLS start signal control
  - Result packaging and return via `data_word`, `data_write`, `data_commit`
- Added internal reset controller
- Added ILA for debug probes:
  - `waveform_wr_out`, `waveform_data_out`
  - FIFO state
  - HLS handshake
  - Result data path

---

##  Testbench / Simulation
- Created `hls4ml_wrapper_tb.v`
  - Functional simulation of:
    - FIFO write
    - HLS module processing
    - Output transmission
- `dummy_hls4ml_ip.v` updated:
  - Added 20-cycle delay between `ap_start` and `ap_done`

---

##  Debugging & Hardware Validation
- ILA used to confirm:
  - `waveform_wr_out` signal correctly asserted
  - FIFO receives data
  - HLS output is valid
- TCP output verified via:
  ```bash
  ./fnetctrl 192.168.1.192 --tcp=hex
  ```
- Issues resolved:
  - `regacc_pst2_done` required for write completion
  - Fixed multiple drivers on `data_reset`
  - Adjusted MMCM/clock signal routes
  - Prevented simulation mismatches due to width issues

---

##  Current Status
- Waveform successfully sent from PC to FPGA
- FIFO collects 100 samples, passed to HLS core
- Result transmitted back via TCP
- Simulation and hardware match expected behavior
