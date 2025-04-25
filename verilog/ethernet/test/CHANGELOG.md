# ✅ CHANGELOG – Waveform Transfer via Fakernet (UDP Register Interface)

## 🗂️ Data Preparation
1. **Created `waveform_data_0.bin`**  
- Each event contains 104 float values  
- Stored as a binary file with 32-bit float values

---

## 🛠️ Software Modifications
2. **New file: `fnetctrl_sendwaveform.c`**  
- Implements `fnet_send_waveform()` to send waveform data via UDP register interface

3. **Extended `fnet_client.c` and `fnet_client.h`**  
- Added `fnet_ctrl_write_register()` for low-level UDP register access

4. **Updated `Makefile`**  
- Included `fnetctrl_sendwaveform.c` in the build targets

5. **Added CLI option `--send-waveform`**  
- Modified `fnetctrl.c` to handle waveform transmission via command-line argument

---

## 🔧 RTL / Vivado Modifications
6. **Modified `fnet_regaccess.vhd`**  
- Added waveform data path and write enable signal:
```vhdl
waveform_data_in <= reg_int_data_wr;
waveform_wr_en   <= '1' when reg_int_write = '1' and reg_int_addr = x"1000" else '0';
```

7. **Modified `fakernet_module.vhd`**  
- Added the following new top-level ports:
```vhdl
waveform_data_in : out std_logic_vector(31 downto 0);
waveform_wr_en   : out std_logic;
```
- Connected them internally to `reg_int_data_wr` and conditionally driven write-enable logic

8. **Modified `efb_common_top.vhd`**  
- Connected `user_data_word`, `user_data_write`, etc. to `fakernet_module` to support waveform input

---

## 🧱 Next Step: FIFO Integration (Planned)
9. **[Upcoming] Integrate FIFO IP Core**
- Connect:
- `waveform_wr_en` → FIFO `wr_en`
- `waveform_data_in` → FIFO `din`
- Add FIFO read-side logic for streaming data into HLS IP (e.g., hls4ml core)
