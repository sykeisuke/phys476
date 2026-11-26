# PHYS476: FPGAs for Physicists (Spring 2027)

Code and data for the course. The slides and lab handouts are handed out separately;
this repository holds only what you run.

## Get the code

```bash
cd ~
git clone https://github.com/sykeisuke/phys476 phys476
mkdir -p ~/phys476_work
```

Copy a chapter's folder into `~/phys476_work` before you change anything, so that
`git pull` never clashes with your edits.

## What is where

| Folder | Contents | Chapters |
|---|---|---|
| `book/code/chNN/` | Verilog, testbenches, XDC, Tcl, Python and HLS C for chapter NN | 0–28 |
| `hls4ml/` | Python environment (`environment.yml`), toy data (`toymc_*.npy`), hls4ml notebooks | 23–27 |
| `final_project/` | Datasets and notebooks for the final-project examples (waveforms, tracks) | 28 |
| `archive/2026/` | 2026 course code, kept for reference; not used in 2027 | — |

## Tool versions

- Vivado / Vitis HLS **2024.1** (matches the PYNQ-Z2 v3.1 and RFSoC 4x2 v3.1.1 images)
- Icarus Verilog for simulation (`iverilog -g2005 -Wall`)
- Python environment: `conda env create -f hls4ml/environment.yml` (Keras 2 / TensorFlow 2.11, hls4ml 1.1.0)

The 2026 version of this repository is the tag `spring2026`.
