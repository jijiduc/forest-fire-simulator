"""
Data export module for video generation.
Prepares simulation data for frame rendering.
"""
import numpy as np
import pandas as pd
from pathlib import Path
from typing import List, Dict, Tuple, Optional
import json
from datetime import datetime


class SimulationDataExporter:
    """Export simulation data at regular intervals for video generation."""
    
    def __init__(self, output_dir: str = "video/output/frames"):
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
    
    def export_for_video(self, 
                        simulation_snapshots: Dict[str, pd.DataFrame],
                        target_duration: float = 15.0,
                        fps: int = 60,
                        scenario_name: str = "baseline") -> Dict:
        """
        Export grid states at regular intervals for video generation.
        
        Args:
            simulation_snapshots: Dict of timestamp -> grid DataFrame
            target_duration: Target video duration in seconds
            fps: Frames per second
            scenario_name: Name of the scenario
            
        Returns:
            Dict with export metadata
        """
        # Calculate required frames
        total_frames = int(target_duration * fps)
        
        # Sort snapshots by time
        sorted_times = sorted([float(t) for t in simulation_snapshots.keys()])
        
        if len(sorted_times) < 2:
            raise ValueError("Need at least 2 snapshots for video generation")
        
        # Calculate time mapping
        sim_duration = sorted_times[-1] - sorted_times[0]
        time_scale = sim_duration / target_duration
        
        # Export metadata
        metadata = {
            'scenario': scenario_name,
            'video_duration': target_duration,
            'fps': fps,
            'total_frames': total_frames,
            'simulation_duration': sim_duration,
            'time_scale': time_scale,
            'frame_data': []
        }
        
        # Generate frame data
        for frame_idx in range(total_frames):
            video_time = frame_idx / fps
            sim_time = sorted_times[0] + video_time * time_scale
            
            # Find nearest snapshots for interpolation
            snapshot_idx = self._find_snapshot_indices(sim_time, sorted_times)
            
            # Get interpolated data
            frame_data = self._interpolate_snapshot(
                sim_time, sorted_times, simulation_snapshots, snapshot_idx
            )
            
            # Save frame data
            frame_filename = f"{scenario_name}_frame_{frame_idx:05d}.json"
            frame_path = self.output_dir / frame_filename
            
            frame_info = {
                'frame_index': frame_idx,
                'video_time': video_time,
                'simulation_time': sim_time,
                'data_file': frame_filename
            }
            
            # Save grid data as JSON for easy loading
            self._save_frame_data(frame_path, frame_data)
            metadata['frame_data'].append(frame_info)
        
        # Save metadata
        metadata_path = self.output_dir / f"{scenario_name}_metadata.json"
        with open(metadata_path, 'w') as f:
            json.dump(metadata, f, indent=2)
        
        return metadata
    
    def _find_snapshot_indices(self, sim_time: float, sorted_times: List[float]) -> Tuple[int, int]:
        """Find the two snapshot indices for interpolation."""
        for i in range(len(sorted_times) - 1):
            if sorted_times[i] <= sim_time <= sorted_times[i + 1]:
                return i, i + 1
        
        # If beyond range, use nearest
        if sim_time < sorted_times[0]:
            return 0, 0
        else:
            return len(sorted_times) - 1, len(sorted_times) - 1
    
    def _interpolate_snapshot(self, 
                             sim_time: float,
                             sorted_times: List[float],
                             snapshots: Dict[str, pd.DataFrame],
                             indices: Tuple[int, int]) -> pd.DataFrame:
        """Interpolate between two snapshots."""
        idx1, idx2 = indices
        
        # If same index, no interpolation needed
        if idx1 == idx2:
            return snapshots[str(sorted_times[idx1])].copy()
        
        t1, t2 = sorted_times[idx1], sorted_times[idx2]
        snapshot1 = snapshots[str(t1)]
        snapshot2 = snapshots[str(t2)]
        
        # Calculate interpolation factor
        alpha = (sim_time - t1) / (t2 - t1) if t2 != t1 else 0
        
        # Create interpolated snapshot
        interpolated = snapshot1.copy()
        
        # Interpolate continuous values
        continuous_cols = ['moisture', 'temperature', 'elevation']
        for col in continuous_cols:
            if col in snapshot1.columns and col in snapshot2.columns:
                interpolated[col] = (1 - alpha) * snapshot1[col] + alpha * snapshot2[col]
        
        # Handle discrete state transitions
        if 'state' in snapshot1.columns:
            # For fire spread, we can show progression
            interpolated['state'] = self._interpolate_states(
                snapshot1['state'], snapshot2['state'], alpha
            )
        
        return interpolated
    
    def _interpolate_states(self, states1: pd.Series, states2: pd.Series, 
                           alpha: float) -> pd.Series:
        """Interpolate between discrete cell states."""
        # Simple approach: switch states based on alpha threshold
        # More sophisticated: probabilistic transition
        interpolated = states1.copy()
        
        # Find cells that changed state
        changed_mask = states1 != states2
        
        # For changed cells, transition based on alpha
        # This creates a wave effect for fire spread
        transition_threshold = 0.5  # Can be randomized for organic look
        
        interpolated[changed_mask & (alpha > transition_threshold)] = states2[changed_mask & (alpha > transition_threshold)]
        
        return interpolated
    
    def _save_frame_data(self, path: Path, data: pd.DataFrame):
        """Save frame data in efficient format."""
        # Convert DataFrame to dict for JSON serialization
        frame_dict = {
            'grid_data': data.to_dict('records'),
            'shape': (data.index.get_level_values('x').max() + 1,
                     data.index.get_level_values('y').max() + 1)
        }
        
        with open(path, 'w') as f:
            json.dump(frame_dict, f)
    
    def prepare_scenario_comparison(self,
                                   baseline_snapshots: Dict[str, pd.DataFrame],
                                   scenario_snapshots: Dict[str, pd.DataFrame],
                                   comparison_start: float = 10.0,
                                   comparison_duration: float = 5.0) -> Dict:
        """
        Prepare data for split-screen scenario comparison.
        
        Args:
            baseline_snapshots: Baseline scenario snapshots
            scenario_snapshots: Alternative scenario snapshots
            comparison_start: When to start comparison in video
            comparison_duration: Duration of comparison segment
            
        Returns:
            Metadata for comparison segment
        """
        # Export both scenarios
        baseline_meta = self.export_for_video(
            baseline_snapshots, 
            target_duration=comparison_start + comparison_duration,
            scenario_name="baseline"
        )
        
        scenario_meta = self.export_for_video(
            scenario_snapshots,
            target_duration=comparison_start + comparison_duration,
            scenario_name="rcp85"
        )
        
        # Create comparison metadata
        comparison_meta = {
            'baseline': baseline_meta,
            'scenario': scenario_meta,
            'comparison_start': comparison_start,
            'comparison_duration': comparison_duration,
            'split_frame_start': int(comparison_start * baseline_meta['fps'])
        }
        
        return comparison_meta