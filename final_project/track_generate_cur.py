import numpy as np
import os
import random

def generate_arc_track(center, radius, start_angle, arc_angle, num_points):
    """
    Generate a list of points along a circular arc.
    center: (cx, cy) tuple
    radius: curvature radius
    start_angle: angle to start from (in radians)
    arc_angle: how wide the arc is (in radians)
    num_points: number of discrete points along the arc
    """
    points = []
    for i in range(num_points):
        theta = start_angle + arc_angle * (i / (num_points - 1))
        x = int(round(center[0] + radius * np.cos(theta)))
        y = int(round(center[1] + radius * np.sin(theta)))
        points.append((x, y))
    return points

def generate_event_tracks(grid_size=30, frame_size=60,
                           track_distribution=(0.05, 0.75, 0.20),
                           noise_prob=0.02):
    """
    Generate one event consisting of arc tracks and noise hits.
    The arc may represent high- or low-momentum particles.
    - High momentum: near-linear arcs, far from grid center
    - Low momentum: curved arcs, more within grid area
    """
    hit_map = np.zeros((grid_size, grid_size), dtype=int)
    label_map = np.zeros((grid_size, grid_size), dtype=int)
    margin = (frame_size - grid_size) // 2
    num_tracks = np.random.choice([0, 1, 2], p=track_distribution)

    tracks = []
    MAX_TRIES = 10  # Retry limit for generating valid track

    for _ in range(num_tracks):
        for attempt in range(MAX_TRIES):
            radius = random.uniform(15, 80)
            arc_angle = random.uniform(np.pi / 6, np.pi / 1.5)
            start_angle = random.uniform(0, 2 * np.pi)
            num_points = random.randint(80, 100)

            offset = int(radius * 0.6)
            cx = random.randint(margin - offset, margin + grid_size + offset)
            cy = random.randint(margin - offset, margin + grid_size + offset)

            track_points = generate_arc_track((cx, cy), radius, start_angle, arc_angle, num_points)

            # Count how many points lie within the 30x30 grid
            in_grid_points = [(x, y) for x, y in track_points
                              if 0 <= x - margin < grid_size and 0 <= y - margin < grid_size]

            if len(in_grid_points) >= 10:
                tracks.append(track_points)
                break

    # Determine which track is longer within visible grid
    visible_lengths = [
        sum(0 <= x - margin < grid_size and 0 <= y - margin < grid_size for x, y in trk)
        for trk in tracks
    ]
    sorted_indices = sorted(range(len(tracks)), key=lambda i: -visible_lengths[i])

    # Assign label 1 to longer track, 2 to shorter
    if len(tracks) == 1:
        label_order = [1]
    elif len(tracks) == 2:
        if visible_lengths[sorted_indices[0]] == visible_lengths[sorted_indices[1]]:
            label_order = random.sample([1, 2], k=2)
        else:
            label_order = [1, 2]
    else:
        label_order = []

    # Fill hit_map and label_map
    for i, track_idx in enumerate(sorted_indices):
        label = label_order[i]
        for x, y in tracks[track_idx]:
            gx, gy = x - margin, y - margin
            if 0 <= gx < grid_size and 0 <= gy < grid_size:
                hit_map[gy, gx] = 1
                label_map[gy, gx] = label

    # Add random noise hits
    for i in range(grid_size):
        for j in range(grid_size):
            if hit_map[i, j] == 0 and random.random() < noise_prob:
                hit_map[i, j] = 1
                label_map[i, j] = 0

    return hit_map, label_map

def generate_dataset(num_events=10000, grid_size=30, out_dir="track_dataset_cur"):
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
    generate_dataset(num_events=10000)

