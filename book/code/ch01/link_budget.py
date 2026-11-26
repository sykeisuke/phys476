# Data rate of a waveform readout
channels = 64        # channels per trigger
samples = 64         # samples per waveform
bits = 12            # bits per sample
trigger_rate = 30e3  # triggers per second

bps = channels * samples * bits * trigger_rate
print(f"{bps/1e9:.2f} Gb/s = {bps/8/1e6:.0f} MB/s")
print(f"1 GbE (1 Gb/s) load: {bps/1e9:.0%}")
