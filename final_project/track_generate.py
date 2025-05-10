import numpy as np
import os
import random

def generate_event_tracks(grid_size=30, frame_size=30,
                          track_distribution=(0.05, 0.75, 0.20),
                          noise_prob=0.02):
    """
    Generate one event with hit and label maps.
    Tracks may start/end on the edges or inside the 30x30 area.
    Longest track is labeled 1, shorter track is labeled 2.
    """
    hit_map = np.zeros((grid_size, grid_size), dtype=int)
    label_map = np.zeros((grid_size, grid_size), dtype=int)
    margin = (frame_size - grid_size) // 2

    num_tracks = np.random.choice([0, 1, 2], p=track_distribution)

    def random_edge_point():
        edge = random.choice(['top', 'bottom', 'left', 'right'])
        if edge == 'top':
            return random.randint(0, frame_size - 1), 0
        elif edge == 'bottom':
            return random.randint(0, frame_size - 1), frame_size - 1
        elif edge == 'left':
            return 0, random.randint(0, frame_size - 1)
        else:
            return frame_size - 1, random.randint(0, frame_size - 1)

    def random_any_point():
        # 50% chance of being on the edge
        return random_edge_point() if random.random() < 0.5 else (
            random.randint(0, frame_size - 1), random.randint(0, frame_size - 1)
        )

    tracks = []
    for _ in range(num_tracks):
        x0, y0 = random_any_point()
        x1, y1 = random_any_point()

        dx = abs(x1 - x0)
        dy = abs(y1 - y0)
        x, y = x0, y0
        sx = 1 if x0 < x1 else -1
        sy = 1 if y0 < y1 else -1

        track_points = []

        if dx > dy:
            err = dx / 2.0
            while x != x1:
                track_points.append((x, y))
                err -= dy
                if err < 0:
                    y += sy
                    err += dx
                x += sx
        else:
            err = dy / 2.0
            while y != y1:
                track_points.append((x, y))
                err -= dx
                if err < 0:
                    x += sx
                    err += dy
                y += sy
        track_points.append((x1, y1))
        tracks.append(track_points)


    # Sort tracks by visible length in 10x10 grid (longer first)
    visible_lengths = [
        sum(0 <= (x - margin) < grid_size and 0 <= (y - margin) < grid_size for x, y in trk)
        for trk in tracks
    ]
    sorted_indices = sorted(range(len(tracks)), key=lambda i: -visible_lengths[i])

    if len(tracks) == 1:
        label_order = [1]
    elif len(tracks) == 2:
        if visible_lengths[sorted_indices[0]] == visible_lengths[sorted_indices[1]]:
            label_order = random.sample([1, 2], k=2)
        else:
            label_order = [1, 2]
    else:
        label_order = []

    for i, track_idx in enumerate(sorted_indices):
        label = label_order[i]
        for x, y in tracks[track_idx]:
            gx, gy = x - margin, y - margin
            if 0 <= gx < grid_size and 0 <= gy < grid_size:
                hit_map[gy, gx] = 1
                label_map[gy, gx] = label

    # Add noise
    for i in range(grid_size):
        for j in range(grid_size):
            if hit_map[i, j] == 0 and random.random() < noise_prob:
                hit_map[i, j] = 1
                label_map[i, j] = 0

    return hit_map, label_map


def generate_dataset(num_events=10000, grid_size=30, out_dir="track_dataset"):
    os.makedirs(out_dir, exist_ok=True)
    hit_maps = np.zeros((num_events, grid_size, grid_size), dtype=np.uint8)
    label_maps = np.zeros((num_events, grid_size, grid_size), dtype=np.uint8)

    for i in range(num_events):
        hit_map, label_map = generate_event_tracks(grid_size=grid_size)
        hit_maps[i] = hit_map
        label_maps[i] = label_map

    np.save(os.path.join(out_dir, "hit_maps.npy"), hit_maps)
    np.save(os.path.join(out_dir, "label_maps.npy"), label_maps)
    print(f"Saved {num_events} events to {out_dir}")


if __name__ == "__main__":
    generate_dataset()
