import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors

# Load data
hit_maps = np.load("track_dataset/hit_maps.npy")
label_maps = np.load("track_dataset/label_maps.npy")

# Define fixed colors: 0=black (noise), 1=red (long), 2=blue (short)
cmap = mcolors.ListedColormap(['black', 'red', 'blue'])
bounds = [0, 0.5, 1.5, 2.5]
norm = mcolors.BoundaryNorm(bounds, cmap.N)

# Display 20 events
for i in range(20):
    fig, axs = plt.subplots(1, 2, figsize=(8, 4))

    axs[0].imshow(hit_maps[i], cmap="gray")
    axs[0].set_title(f"Hit Map - Event {i}")
    axs[0].axis("off")

    axs[1].imshow(label_maps[i], cmap=cmap, norm=norm)
    axs[1].set_title(f"Label Map - Event {i} (Red=Track1, Blue=Track2)")
    axs[1].axis("off")

    plt.tight_layout()
    plt.show()
    input(f"[Event {i}] → Press Enter to show next...")
