#!/usr/bin/env python3
"""
Frame generator for forest fire simulation video
Generates 2D grid visualization frames from CSV exports
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.colors import LinearSegmentedColormap
from pathlib import Path
import multiprocessing as mp
from functools import partial
import argparse

# Color scheme for cell states
CELL_COLORS = {
    'empty': (0.83, 0.83, 0.83),      # Light gray
    'tree': (0.13, 0.55, 0.13),       # Forest green
    'burning_0.1': (1.0, 0.87, 0.0),  # Gold (low intensity)
    'burning_0.3': (1.0, 0.65, 0.0),  # Orange
    'burning_0.5': (1.0, 0.5, 0.0),   # Dark orange
    'burning_0.7': (1.0, 0.27, 0.0),  # Red-orange
    'burning_0.9': (1.0, 0.0, 0.0),   # Red (high intensity)
    'burning_1.0': (0.8, 0.0, 0.0),   # Dark red
    'burnt': (0.35, 0.16, 0.14),      # Dark brown
    'water': (0.25, 0.64, 0.87),      # Light blue
    'rock': (0.5, 0.5, 0.5)           # Gray
}

def create_colormap():
    """Create custom colormap for burning intensities"""
    burning_colors = [
        (1.0, 0.87, 0.0),  # Gold
        (1.0, 0.65, 0.0),  # Orange
        (1.0, 0.27, 0.0),  # Red-orange
        (1.0, 0.0, 0.0),   # Red
    ]
    n_bins = 100
    cmap = LinearSegmentedColormap.from_list('burning', burning_colors, N=n_bins)
    return cmap

def load_frame_data(frame_path):
    """Load frame data from CSV"""
    df = pd.read_csv(frame_path)
    return df

def load_frame_metrics(metrics_path):
    """Load metrics for the frame"""
    if metrics_path.exists():
        df = pd.read_csv(metrics_path)
        return df.iloc[0].to_dict()
    return None

def create_2d_frame(frame_data, metrics, frame_number, output_path, config):
    """Generate a single 2D visualization frame"""
    
    # Set up figure with specific DPI for 1080p
    fig_width = 19.2  # 1920 pixels at 100 DPI
    fig_height = 10.8  # 1080 pixels at 100 DPI
    fig, ax = plt.subplots(1, 1, figsize=(fig_width, fig_height), dpi=100)
    
    # Get grid dimensions
    width = frame_data['x'].max() + 1
    height = frame_data['y'].max() + 1
    
    # Create grid array
    grid = np.zeros((height, width, 3))
    
    # Fill grid with colors
    for _, cell in frame_data.iterrows():
        x, y = int(cell['x']), int(cell['y'])
        state = cell['state']
        
        # Handle burning states with intensity gradient
        if state.startswith('burning_'):
            intensity = float(state.split('_')[1])
            # Interpolate color based on intensity
            if intensity <= 0.3:
                color = interpolate_color(CELL_COLORS['tree'], CELL_COLORS['burning_0.3'], intensity/0.3)
            elif intensity <= 0.7:
                color = interpolate_color(CELL_COLORS['burning_0.3'], CELL_COLORS['burning_0.7'], (intensity-0.3)/0.4)
            else:
                color = interpolate_color(CELL_COLORS['burning_0.7'], CELL_COLORS['burning_1.0'], (intensity-0.7)/0.3)
        else:
            color = CELL_COLORS.get(state, CELL_COLORS['empty'])
        
        grid[y, x] = color
    
    # Display grid
    ax.imshow(grid, origin='lower', interpolation='nearest')
    
    # Remove axes for cleaner look
    ax.set_xticks([])
    ax.set_yticks([])
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    ax.spines['left'].set_visible(False)
    
    # Add title
    title = f"Forest Fire Simulation - Swiss Alps"
    ax.text(0.5, 0.98, title, transform=ax.transAxes, 
            fontsize=24, weight='bold', ha='center', va='top',
            color='white', bbox=dict(boxstyle='round,pad=0.5', facecolor='black', alpha=0.7))
    
    # Add metrics panel if available
    if metrics:
        add_metrics_panel(ax, metrics, frame_number)
    
    # Add scale bar
    add_scale_bar(ax, width, height)
    
    # Save frame
    plt.savefig(output_path, dpi=100, bbox_inches='tight', pad_inches=0.1, facecolor='black')
    plt.close()

def interpolate_color(color1, color2, t):
    """Linear interpolation between two colors"""
    return tuple(c1 * (1 - t) + c2 * t for c1, c2 in zip(color1, color2))

def add_metrics_panel(ax, metrics, frame_number):
    """Add metrics overlay panel"""
    panel_text = [
        f"Time: {metrics.get('time', 0):.1f} hours",
        f"Trees: {metrics.get('tree_cells', 0):,}",
        f"Burning: {metrics.get('burning_cells', 0):,}",
        f"Burnt: {metrics.get('burnt_cells', 0):,}",
        f"Frame: {frame_number}"
    ]
    
    panel_str = '\n'.join(panel_text)
    
    # Create semi-transparent panel
    ax.text(0.02, 0.02, panel_str, transform=ax.transAxes,
            fontsize=14, ha='left', va='bottom',
            color='white', family='monospace',
            bbox=dict(boxstyle='round,pad=0.5', facecolor='black', alpha=0.7))

def add_scale_bar(ax, width, height):
    """Add scale bar to the plot"""
    # Assuming each cell is 30m x 30m (typical for forest fire models)
    cell_size = 30  # meters
    scale_length = 10  # number of cells for scale bar
    scale_meters = scale_length * cell_size
    
    # Position scale bar in bottom right
    x_pos = width * 0.85
    y_pos = height * 0.05
    
    # Draw scale bar
    rect = patches.Rectangle((x_pos, y_pos), scale_length, 1, 
                           linewidth=0, edgecolor='none', facecolor='white')
    ax.add_patch(rect)
    
    # Add scale text
    ax.text(x_pos + scale_length/2, y_pos + 2, f'{scale_meters}m',
            ha='center', va='bottom', color='white', fontsize=12, weight='bold',
            bbox=dict(boxstyle='round,pad=0.3', facecolor='black', alpha=0.7))

def generate_frame_wrapper(args):
    """Wrapper for multiprocessing"""
    frame_num, input_dir, output_dir, config = args
    
    frame_path = input_dir / f"frames/frame_{frame_num:06d}.csv"
    metrics_path = input_dir / f"frames/metrics_{frame_num:06d}.csv"
    output_path = output_dir / f"frame_{frame_num:06d}.png"
    
    if not frame_path.exists():
        print(f"Warning: Frame {frame_num} not found")
        return
    
    frame_data = load_frame_data(frame_path)
    metrics = load_frame_metrics(metrics_path)
    
    create_2d_frame(frame_data, metrics, frame_num, output_path, config)
    print(f"Generated frame {frame_num}")

def generate_all_frames(input_dir, output_dir, config, num_processes=None):
    """Generate all frames in parallel"""
    input_path = Path(input_dir)
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)
    
    # Find all frame files
    frame_files = sorted(input_path.glob("frames/frame_*.csv"))
    frame_numbers = [int(f.stem.split('_')[1]) for f in frame_files]
    
    print(f"Found {len(frame_numbers)} frames to process")
    
    # Prepare arguments for parallel processing
    args_list = [(num, input_path, output_path, config) for num in frame_numbers]
    
    # Process frames in parallel
    if num_processes is None:
        num_processes = mp.cpu_count()
    
    with mp.Pool(processes=num_processes) as pool:
        pool.map(generate_frame_wrapper, args_list)
    
    print(f"Generated {len(frame_numbers)} frames")

def main():
    parser = argparse.ArgumentParser(description='Generate 2D visualization frames')
    parser.add_argument('input_dir', help='Input directory with CSV exports')
    parser.add_argument('output_dir', help='Output directory for PNG frames')
    parser.add_argument('--processes', type=int, help='Number of parallel processes')
    
    args = parser.parse_args()
    
    config = {
        'show_wind': True,
        'show_elevation': False,
        'color_scheme': 'default'
    }
    
    generate_all_frames(args.input_dir, args.output_dir, config, args.processes)

if __name__ == '__main__':
    main()