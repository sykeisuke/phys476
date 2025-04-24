import numpy as np

data = np.load('waveform_data_0.npy')  # shape: (10000, 100)
data.astype(np.float32).tofile('waveform_data_0.bin')
