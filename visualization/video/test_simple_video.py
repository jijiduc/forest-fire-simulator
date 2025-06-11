#!/usr/bin/env python3
"""
Simple test script for video generation.
"""
import sys
from pathlib import Path
import numpy as np
import pandas as pd

sys.path.append(str(Path(__file__).parent))
sys.path.append(str(Path(__file__).parent.parent / 'python'))

from scripts.frame_generator import GridFrameRenderer
from scripts.video_assembler import VideoAssembler, VideoConfig


def create_test_frames(n_frames=60, grid_size=30):
    """Create simple test frames."""
    renderer = GridFrameRenderer(resolution=(640, 480), dpi=100)
    frames = []
    
    for i in range(n_frames):
        # Create simple expanding fire pattern
        grid_data = []
        fire_radius = i / 3
        
        for x in range(grid_size):
            for y in range(grid_size):
                dist = np.sqrt((x - grid_size/2)**2 + (y - grid_size/2)**2)
                
                if dist < fire_radius - 2:
                    state = 'Burnt'
                elif dist < fire_radius:
                    state = 'Burning'
                elif dist < fire_radius + 5:
                    state = 'Tree'
                else:
                    state = 'Empty'
                
                grid_data.append({
                    'x': x,
                    'y': y,
                    'state': state
                })
        
        df = pd.DataFrame(grid_data)
        df.set_index(['x', 'y'], inplace=True)
        
        # Render frame
        frame = renderer.render_frame(df, i)
        frames.append(frame)
        
        if i % 10 == 0:
            print(f"Generated frame {i}/{n_frames}")
    
    return frames


def main():
    print("Creating test video...")
    
    # Generate test frames
    frames = create_test_frames(60)  # 1 second at 60fps
    
    # Create video
    assembler = VideoAssembler()
    config = VideoConfig(
        resolution=(640, 480),
        fps=60,
        crf=23,
        preset='fast'
    )
    
    video_path = assembler.create_video_from_frames(
        frames, "test_simple", config
    )
    
    if video_path:
        print(f"Success! Video created: {video_path}")
    else:
        print("Failed to create video")


if __name__ == "__main__":
    main()