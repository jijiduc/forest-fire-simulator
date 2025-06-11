#!/usr/bin/env python3
"""
Main script to generate the 15-second demo video for forest fire simulation.
"""
import sys
import argparse
from pathlib import Path
import json
import numpy as np
import pandas as pd
from typing import Dict, List
import warnings

# Add paths for imports
sys.path.append(str(Path(__file__).parent))
sys.path.append(str(Path(__file__).parent.parent / 'python'))

from scripts.data_exporter import SimulationDataExporter
from scripts.interpolator import FrameInterpolator
from scripts.segment_composer import VideoSegmentComposer, SegmentConfig
from scripts.video_assembler import VideoAssembler, VideoConfig, VideoQualityChecker

# Import data loading utilities
from utils.data_loader import load_all_snapshots, load_simulation_output


class DemoVideoGenerator:
    """Generate the 15-second demo video with all segments."""
    
    def __init__(self, output_dir: str = "video/output"):
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        self.data_exporter = SimulationDataExporter()
        self.interpolator = FrameInterpolator()
        self.segment_composer = VideoSegmentComposer()
        self.video_assembler = VideoAssembler()
        self.quality_checker = VideoQualityChecker()
    
    def generate_demo_video(self,
                           simulation_output_dir: str,
                           output_name: str = "forest_fire_demo_15s_1080p60",
                           use_sample_data: bool = False) -> str:
        """
        Generate the complete 15-second demo video.
        
        Args:
            simulation_output_dir: Directory with simulation outputs
            output_name: Output video filename
            use_sample_data: Whether to use sample data for testing
            
        Returns:
            Path to generated video
        """
        print("=== Forest Fire Demo Video Generator ===")
        print(f"Output: {output_name}.mp4")
        print(f"Duration: 15 seconds @ 60fps (900 frames)")
        print()
        
        # Load simulation data
        if use_sample_data:
            print("Generating sample simulation data...")
            snapshots = self._generate_sample_snapshots()
        else:
            print(f"Loading simulation data from {simulation_output_dir}...")
            snapshots = self._load_simulation_snapshots(simulation_output_dir)
        
        if not snapshots:
            print("Error: No simulation data found!")
            return None
        
        print(f"Loaded {len(snapshots)} snapshots")
        
        # Configure video settings
        video_config = VideoConfig(
            resolution=(1920, 1080),
            fps=60,
            codec='libx264',
            crf=18,
            preset='slow'
        )
        
        # Generate video segments
        all_frames = []
        
        # Segment 1: Opening (0-5s) - Ignition and initial spread
        print("\nGenerating Segment 1: Opening (0-5s)...")
        opening_frames = self.segment_composer.compose_opening_segment(
            snapshots, duration=5.0, fps=video_config.fps
        )
        all_frames.extend(opening_frames)
        print(f"  Generated {len(opening_frames)} frames")
        
        # Segment 2: Middle (5-10s) - Critical transition with 3D
        print("\nGenerating Segment 2: Middle (5-10s)...")
        middle_frames = self.segment_composer.compose_middle_segment(
            snapshots, duration=5.0, start_time=5.0, fps=video_config.fps
        )
        all_frames.extend(middle_frames)
        print(f"  Generated {len(middle_frames)} frames")
        
        # Segment 3: Finale (10-15s) - Climate comparison
        print("\nGenerating Segment 3: Finale (10-15s)...")
        
        # For comparison, we need two scenarios
        if use_sample_data:
            scenario_snapshots = self._generate_sample_snapshots(scenario='rcp85')
        else:
            # Try to load RCP8.5 scenario
            scenario_snapshots = self._load_simulation_snapshots(
                simulation_output_dir, scenario='rcp85'
            )
            
        if not scenario_snapshots:
            print("  Warning: No scenario data found, using modified baseline")
            scenario_snapshots = self._modify_snapshots_for_scenario(snapshots)
        
        finale_frames = self.segment_composer.compose_finale_segment(
            snapshots, scenario_snapshots,
            duration=5.0, start_time=10.0, fps=video_config.fps
        )
        all_frames.extend(finale_frames)
        print(f"  Generated {len(finale_frames)} frames")
        
        print(f"\nTotal frames: {len(all_frames)}")
        
        # Create video
        print("\nAssembling video...")
        video_path = self.video_assembler.create_video_from_frames(
            all_frames, output_name, video_config
        )
        
        if video_path:
            print(f"\nVideo created: {video_path}")
            
            # Validate video
            print("\nValidating video quality...")
            report = self.quality_checker.validate_video(video_path)
            
            print(f"  Duration: {report['duration']:.1f}s")
            print(f"  Resolution: {report['resolution']}")
            print(f"  FPS: {report['fps']}")
            print(f"  Size: {report['size_mb']:.1f}MB")
            
            if report['valid']:
                print("  ✓ Video meets all specifications!")
            else:
                print("  ✗ Issues found:")
                for issue in report['issues']:
                    print(f"    - {issue}")
            
            # Create additional formats
            print("\nCreating additional formats...")
            variants = self.video_assembler.create_video_variants(
                video_path, create_webm=True, create_gif=True
            )
            
            for fmt, path in variants.items():
                if path and Path(path).exists():
                    print(f"  Created {fmt}: {path}")
            
            return video_path
        else:
            print("\nError: Video creation failed!")
            return None
    
    def _load_simulation_snapshots(self, 
                                  output_dir: str,
                                  scenario: str = 'baseline') -> Dict[str, pd.DataFrame]:
        """Load simulation snapshots from output directory."""
        output_path = Path(output_dir)
        
        # Look for grid snapshots
        pattern = f"*{scenario}*_grid.csv" if scenario != 'baseline' else "*_grid.csv"
        snapshots = load_all_snapshots(str(output_path), pattern)
        
        # Convert to timestamped dict
        timestamped = {}
        for i, (name, data) in enumerate(snapshots.items()):
            # Try to extract time from filename or use index
            time = float(i)  # Simple indexing for now
            timestamped[str(time)] = data
        
        return timestamped
    
    def _generate_sample_snapshots(self, 
                                  n_snapshots: int = 20,
                                  grid_size: int = 50,
                                  scenario: str = 'baseline') -> Dict[str, pd.DataFrame]:
        """Generate sample snapshots for testing."""
        snapshots = {}
        
        # Adjust parameters based on scenario
        if scenario == 'rcp85':
            spread_rate = 1.5  # Faster spread
            ignition_prob = 0.8  # Higher ignition
        else:
            spread_rate = 1.0
            ignition_prob = 0.6
        
        for t in range(n_snapshots):
            grid_data = []
            
            # Create fire pattern that grows over time
            fire_radius = min(t * spread_rate, grid_size // 3)
            fire_center = (grid_size // 2, grid_size // 2)
            
            for x in range(grid_size):
                for y in range(grid_size):
                    # Distance from fire center
                    dist = np.sqrt((x - fire_center[0])**2 + (y - fire_center[1])**2)
                    
                    # Determine state based on distance and time
                    if dist < fire_radius * 0.7:
                        state = 'Burnt'
                    elif dist < fire_radius:
                        state = 'Burning'
                    elif dist < fire_radius + 5:
                        state = 'Tree' if np.random.random() > 0.1 else 'Empty'
                    else:
                        state = 'Tree' if np.random.random() > 0.2 else 'Empty'
                    
                    # Create realistic elevation
                    elevation = 500 + 50 * np.sin(x/10) + 30 * np.cos(y/8) + np.random.normal(0, 5)
                    
                    grid_data.append({
                        'x': x,
                        'y': y,
                        'state': state,
                        'elevation': elevation,
                        'moisture': 0.3 + 0.2 * np.random.random(),
                        'temperature': 25 + 5 * np.random.random()
                    })
            
            df = pd.DataFrame(grid_data)
            df.set_index(['x', 'y'], inplace=True)
            snapshots[str(float(t))] = df
        
        return snapshots
    
    def _modify_snapshots_for_scenario(self, 
                                      baseline_snapshots: Dict[str, pd.DataFrame],
                                      scenario: str = 'rcp85') -> Dict[str, pd.DataFrame]:
        """Modify baseline snapshots to simulate different scenario."""
        modified = {}
        
        for time_key, snapshot in baseline_snapshots.items():
            modified_snap = snapshot.copy()
            
            # Increase fire spread for climate scenario
            if 'state' in modified_snap.columns:
                # Convert more trees to burning
                tree_mask = modified_snap['state'] == 'Tree'
                n_trees = tree_mask.sum()
                
                # Randomly ignite more trees
                if n_trees > 0:
                    ignite_prob = 0.1 if scenario == 'rcp85' else 0.05
                    ignite_mask = tree_mask & (np.random.random(len(modified_snap)) < ignite_prob)
                    modified_snap.loc[ignite_mask, 'state'] = 'Burning'
            
            # Adjust environmental parameters
            if 'temperature' in modified_snap.columns:
                modified_snap['temperature'] += 5  # Higher temperature
            
            if 'moisture' in modified_snap.columns:
                modified_snap['moisture'] *= 0.7  # Lower moisture
            
            modified[time_key] = modified_snap
        
        return modified


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Generate 15-second forest fire demo video"
    )
    parser.add_argument(
        '--input', '-i',
        default='../../output',
        help='Simulation output directory (default: ../../output)'
    )
    parser.add_argument(
        '--output', '-o',
        default='forest_fire_demo_15s_1080p60',
        help='Output video name (default: forest_fire_demo_15s_1080p60)'
    )
    parser.add_argument(
        '--sample', '-s',
        action='store_true',
        help='Use sample data for testing'
    )
    parser.add_argument(
        '--quick',
        action='store_true',
        help='Quick mode: lower quality for faster processing'
    )
    
    args = parser.parse_args()
    
    # Create generator
    generator = DemoVideoGenerator()
    
    # Adjust settings for quick mode
    if args.quick:
        # Override with faster settings
        VideoConfig.preset = 'fast'
        VideoConfig.crf = 23
        print("Using quick mode (lower quality, faster processing)")
    
    # Generate video
    video_path = generator.generate_demo_video(
        args.input,
        args.output,
        use_sample_data=args.sample
    )
    
    if video_path:
        print(f"\n✓ Success! Video saved to: {video_path}")
        print("\nTo play the video:")
        print(f"  ffplay {video_path}")
        print(f"  vlc {video_path}")
        print(f"  mpv {video_path}")
    else:
        print("\n✗ Video generation failed!")
        sys.exit(1)


if __name__ == "__main__":
    main()