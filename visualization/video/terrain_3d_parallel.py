#!/usr/bin/env python3
"""
Parallel 3D frame generation for faster processing
"""

import multiprocessing as mp
from pathlib import Path
from terrain_3d import EnhancedTerrain3DRenderer
import argparse

def render_frame_wrapper(args):
    """Wrapper for multiprocessing"""
    frame_num, input_dir, output_dir, total_frames, elevation_path = args
    
    # Create renderer for this process
    renderer = EnhancedTerrain3DRenderer(elevation_path)
    renderer.render_frame(frame_num, input_dir, output_dir, total_frames)

def generate_3d_frames_parallel(input_dir, output_dir, num_frames=None, num_processes=None):
    """Generate all 3D frames in parallel"""
    input_path = Path(input_dir)
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)
    
    # Load elevation data path
    elevation_path = input_path / "metadata/elevation.csv"
    if not elevation_path.exists():
        print("Error: Elevation data not found")
        return
    
    # Find all frame files
    frame_files = sorted(input_path.glob("frames/frame_*.csv"))
    frame_numbers = [int(f.stem.split('_')[1]) for f in frame_files]
    
    if num_frames:
        frame_numbers = frame_numbers[:num_frames]
    
    print(f"Rendering {len(frame_numbers)} 3D frames")
    
    # Prepare arguments for parallel processing
    args_list = [(num, input_path, output_path, len(frame_numbers), elevation_path) 
                 for num in frame_numbers]
    
    # Process frames in parallel
    if num_processes is None:
        num_processes = min(mp.cpu_count(), 4)  # Limit to 4 processes for memory
    
    with mp.Pool(processes=num_processes) as pool:
        pool.map(render_frame_wrapper, args_list)
    
    print(f"Completed {len(frame_numbers)} 3D frames")

def main():
    parser = argparse.ArgumentParser(description='Generate 3D frames in parallel')
    parser.add_argument('input_dir', help='Input directory with CSV exports')
    parser.add_argument('output_dir', help='Output directory for 3D frames')
    parser.add_argument('--frames', type=int, help='Number of frames to render')
    parser.add_argument('--processes', type=int, help='Number of parallel processes')
    
    args = parser.parse_args()
    generate_3d_frames_parallel(args.input_dir, args.output_dir, args.frames, args.processes)

if __name__ == '__main__':
    main()