"""
Video segment composition for creating the final demo video.
"""
import numpy as np
import pandas as pd
from pathlib import Path
from typing import List, Dict, Tuple, Optional
import json

from .frame_generator import GridFrameRenderer, TerrainFrameRenderer, OverlayRenderer
from .camera_paths import CameraPathController
from .transitions import TransitionEffects
from .interpolator import FrameInterpolator


class SegmentConfig:
    """Configuration for a video segment."""
    
    def __init__(self,
                 name: str,
                 duration: float,
                 start_time: float,
                 render_type: str = '2d',
                 camera_path: Optional[str] = None,
                 transition_in: Optional[str] = None,
                 transition_out: Optional[str] = None):
        self.name = name
        self.duration = duration
        self.start_time = start_time
        self.render_type = render_type
        self.camera_path = camera_path
        self.transition_in = transition_in
        self.transition_out = transition_out


class VideoSegmentComposer:
    """Compose video segments with specific characteristics."""
    
    def __init__(self, output_dir: str = "video/output/segments"):
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        self.grid_renderer = GridFrameRenderer()
        self.terrain_renderer = TerrainFrameRenderer()
        self.overlay_renderer = OverlayRenderer()
        self.camera_controller = CameraPathController()
        self.transitions = TransitionEffects()
        self.interpolator = FrameInterpolator()
    
    def compose_segment(self,
                       frames_data: List[Dict],
                       segment_config: SegmentConfig,
                       fps: int = 60) -> List[np.ndarray]:
        """
        Compose a video segment with specific characteristics.
        
        Args:
            frames_data: List of frame data dictionaries
            segment_config: Segment configuration
            fps: Frames per second
            
        Returns:
            List of composed frames
        """
        composed_frames = []
        
        # Get camera path if 3D
        camera_positions = None
        if segment_config.render_type == '3d' and segment_config.camera_path:
            camera_positions = self.camera_controller.paths[segment_config.camera_path](
                segment_config.duration, fps
            )
        
        # Render frames based on type
        for i, frame_data in enumerate(frames_data):
            if segment_config.render_type == '2d':
                frame = self._render_2d_frame(frame_data, i)
            elif segment_config.render_type == '3d':
                camera_pos = camera_positions[i] if camera_positions else {
                    'azimuth': -45, 'elevation': 30, 'distance': 10
                }
                frame = self._render_3d_frame(frame_data, camera_pos)
            elif segment_config.render_type == 'split':
                frame = self._render_split_frame(frame_data, i)
            else:
                frame = self._render_2d_frame(frame_data, i)
            
            # Add overlays
            metrics = self._calculate_metrics(frame_data)
            time_info = {
                'video_time': segment_config.start_time + (i / fps),
                'frame': i
            }
            
            frame = self.overlay_renderer.add_overlays(
                frame, metrics, time_info, style='minimal'
            )
            
            composed_frames.append(frame)
        
        # Apply transitions
        if segment_config.transition_in:
            composed_frames = self._apply_transition_in(
                composed_frames, segment_config.transition_in, fps
            )
        
        if segment_config.transition_out:
            composed_frames = self._apply_transition_out(
                composed_frames, segment_config.transition_out, fps
            )
        
        return composed_frames
    
    def compose_opening_segment(self,
                               simulation_data: Dict,
                               duration: float = 5.0,
                               fps: int = 60) -> List[np.ndarray]:
        """
        Compose opening segment (0-5s): Ignition and initial spread.
        
        Features:
        - Fade in from black
        - Zoom from overview to fire location
        - Accelerated time initially
        """
        frames = []
        total_frames = int(duration * fps)
        
        # Load frame data
        frame_data_list = self._load_frame_data(simulation_data, 0, duration)
        
        # Create zoom effect
        zoom_duration = 2.0  # seconds
        zoom_frames = int(zoom_duration * fps)
        
        for i in range(total_frames):
            # Load grid data
            frame_data = frame_data_list[min(i, len(frame_data_list) - 1)]
            
            # Render frame
            if i < zoom_frames:
                # Zoom effect
                zoom_progress = i / zoom_frames
                frame = self._render_zoom_frame(frame_data, zoom_progress)
            else:
                # Normal 2D view
                frame = self._render_2d_frame(frame_data, i)
            
            # Add overlays
            metrics = self._calculate_metrics(frame_data)
            time_info = {
                'video_time': i / fps,
                'frame': i,
                'scenario': 'Forest Fire Simulation'
            }
            
            frame = self.overlay_renderer.add_overlays(
                frame, metrics, time_info, style='minimal'
            )
            
            # Fade in effect for first second
            if i < fps:
                fade_progress = i / fps
                frame = self.transitions.fade_from_black(frame, fade_progress)
            
            # Add legend in corner
            if i > fps * 2:  # After 2 seconds
                frame = self.overlay_renderer.add_legend(frame, 'bottom-right')
            
            frames.append(frame)
        
        return frames
    
    def compose_middle_segment(self,
                              simulation_data: Dict,
                              duration: float = 5.0,
                              start_time: float = 5.0,
                              fps: int = 60) -> List[np.ndarray]:
        """
        Compose middle segment (5-10s): Critical transition.
        
        Features:
        - Switch to 3D view
        - Rotating camera showing terrain impact
        - Highlight percolation transition
        """
        frames = []
        total_frames = int(duration * fps)
        
        # Load frame data
        frame_data_list = self._load_frame_data(
            simulation_data, start_time, start_time + duration
        )
        
        # Get camera path
        camera_positions = self.camera_controller.paths['orbit'](duration, fps)
        
        for i in range(total_frames):
            # Load grid data
            frame_data = frame_data_list[min(i, len(frame_data_list) - 1)]
            
            # Render 3D frame
            camera_pos = camera_positions[i]
            frame = self._render_3d_frame(frame_data, camera_pos)
            
            # Calculate metrics
            metrics = self._calculate_metrics(frame_data)
            
            # Highlight percolation transition
            if metrics.get('percolation', 0) > 0.5:
                time_info = {
                    'video_time': start_time + (i / fps),
                    'frame': i,
                    'scenario': 'CRITICAL TRANSITION'
                }
            else:
                time_info = {
                    'video_time': start_time + (i / fps),
                    'frame': i
                }
            
            frame = self.overlay_renderer.add_overlays(
                frame, metrics, time_info, style='full'
            )
            
            # Add visual effect during transition
            if 0.4 < metrics.get('percolation', 0) < 0.6:
                frame = self.transitions.apply_vignette(frame, strength=0.3)
            
            frames.append(frame)
        
        return frames
    
    def compose_finale_segment(self,
                              baseline_data: Dict,
                              scenario_data: Dict,
                              duration: float = 5.0,
                              start_time: float = 10.0,
                              fps: int = 60) -> List[np.ndarray]:
        """
        Compose finale segment (10-15s): Climate comparison.
        
        Features:
        - Split screen transition
        - Synchronized simulations
        - Final statistics overlay
        """
        frames = []
        total_frames = int(duration * fps)
        
        # Load frame data for both scenarios
        baseline_frames = self._load_frame_data(
            baseline_data, start_time, start_time + duration
        )
        scenario_frames = self._load_frame_data(
            scenario_data, start_time, start_time + duration
        )
        
        # Transition to split screen over 1 second
        transition_frames = fps
        
        for i in range(total_frames):
            # Get frame data
            baseline_frame_data = baseline_frames[min(i, len(baseline_frames) - 1)]
            scenario_frame_data = scenario_frames[min(i, len(scenario_frames) - 1)]
            
            # Render frames
            baseline_render = self._render_2d_frame(baseline_frame_data, i)
            scenario_render = self._render_2d_frame(scenario_frame_data, i)
            
            # Apply split screen transition
            if i < transition_frames:
                # Wipe transition
                progress = i / transition_frames
                frame = self.transitions.split_screen_wipe(
                    baseline_render, scenario_render, progress, 'horizontal'
                )
            else:
                # Full split screen
                frame = self.transitions.create_split_screen(
                    baseline_render, scenario_render, 'vertical'
                )
            
            # Add overlays for each side
            baseline_metrics = self._calculate_metrics(baseline_frame_data)
            scenario_metrics = self._calculate_metrics(scenario_frame_data)
            
            # Create comparison metrics
            time_info = {
                'video_time': start_time + (i / fps),
                'frame': i
            }
            
            # Add labels
            if i >= transition_frames:
                frame = self._add_split_screen_labels(
                    frame, 'BASELINE', 'RCP 8.5 (2100)'
                )
                
                # Add final statistics in last second
                if i >= total_frames - fps:
                    frame = self._add_final_statistics(
                        frame, baseline_metrics, scenario_metrics
                    )
            
            frames.append(frame)
        
        # Fade to black in last 0.5 seconds
        fade_start = total_frames - int(0.5 * fps)
        for i in range(fade_start, total_frames):
            progress = (i - fade_start) / (total_frames - fade_start)
            frames[i] = self.transitions.fade_to_black(frames[i], progress)
        
        return frames
    
    def _render_2d_frame(self, frame_data: Dict, frame_number: int) -> np.ndarray:
        """Render a 2D grid frame."""
        grid_df = self._dict_to_dataframe(frame_data['grid_data'])
        return self.grid_renderer.render_frame(grid_df, frame_number)
    
    def _render_3d_frame(self, frame_data: Dict, camera_pos: Dict) -> np.ndarray:
        """Render a 3D terrain frame."""
        grid_df = self._dict_to_dataframe(frame_data['grid_data'])
        return self.terrain_renderer.render_3d_frame(grid_df, camera_pos)
    
    def _render_split_frame(self, frame_data: Dict, frame_number: int) -> np.ndarray:
        """Render split screen frame."""
        # This would be implemented for side-by-side comparison
        grid_df = self._dict_to_dataframe(frame_data['grid_data'])
        return self.grid_renderer.render_frame(grid_df, frame_number, layout='left')
    
    def _render_zoom_frame(self, frame_data: Dict, zoom_progress: float) -> np.ndarray:
        """Render frame with zoom effect."""
        grid_df = self._dict_to_dataframe(frame_data['grid_data'])
        
        # Find fire location for zoom target
        burning_cells = grid_df[grid_df['state'] == 'Burning']
        if not burning_cells.empty:
            # Get center of burning area
            x_center = burning_cells.index.get_level_values('x').mean()
            y_center = burning_cells.index.get_level_values('y').mean()
        else:
            # Default to center
            x_center = grid_df.index.get_level_values('x').max() / 2
            y_center = grid_df.index.get_level_values('y').max() / 2
        
        # Render full frame
        frame = self.grid_renderer.render_frame(grid_df, 0)
        
        # Apply zoom
        zoom_factor = 1 + (2 - 1) * (1 - zoom_progress)  # Zoom out from 2x to 1x
        zoomed_frames = self.transitions.zoom_transition(
            [frame], (int(x_center * 10), int(y_center * 10)), zoom_factor
        )
        
        return zoomed_frames[0]
    
    def _calculate_metrics(self, frame_data: Dict) -> Dict:
        """Calculate metrics from frame data."""
        grid_data = frame_data['grid_data']
        
        # Count states
        state_counts = {'Empty': 0, 'Tree': 0, 'Burning': 0, 'Burnt': 0}
        for cell in grid_data:
            state = cell.get('state', 'Empty')
            state_counts[state] = state_counts.get(state, 0) + 1
        
        total_cells = len(grid_data)
        tree_cells = state_counts['Tree'] + state_counts['Burning'] + state_counts['Burnt']
        
        metrics = {
            'active_fires': state_counts['Burning'],
            'burnt_fraction': state_counts['Burnt'] / tree_cells if tree_cells > 0 else 0,
            'percolation': 0.0  # Would need actual calculation
        }
        
        return metrics
    
    def _dict_to_dataframe(self, grid_data: List[Dict]) -> pd.DataFrame:
        """Convert grid data dict to DataFrame."""
        df = pd.DataFrame(grid_data)
        if 'x' in df.columns and 'y' in df.columns:
            df.set_index(['x', 'y'], inplace=True)
        return df
    
    def _load_frame_data(self, simulation_data: Dict, 
                        start_time: float, end_time: float) -> List[Dict]:
        """Load frame data for time range."""
        frames = []
        
        # Convert simulation snapshots to frame data format
        n_frames = int((end_time - start_time) * 60)  # 60 fps
        
        # Get available snapshots
        snapshot_times = sorted([float(t) for t in simulation_data.keys()])
        
        for i in range(n_frames):
            frame_time = start_time + (i / 60.0)
            
            # Find closest snapshot
            closest_time = min(snapshot_times, key=lambda t: abs(t - frame_time))
            snapshot = simulation_data[str(closest_time)]
            
            # Convert to expected format
            frame_data = {
                'grid_data': snapshot.to_dict('records') if hasattr(snapshot, 'to_dict') else snapshot,
                'time': frame_time
            }
            frames.append(frame_data)
        
        return frames
    
    def _add_split_screen_labels(self, frame: np.ndarray,
                                left_label: str, right_label: str) -> np.ndarray:
        """Add labels to split screen."""
        height, width = frame.shape[:2]
        
        # This would add text labels to each side
        # Implementation would use matplotlib or PIL
        
        return frame
    
    def _add_final_statistics(self, frame: np.ndarray,
                             baseline_metrics: Dict,
                             scenario_metrics: Dict) -> np.ndarray:
        """Add final statistics overlay."""
        # This would add a statistics box showing comparison
        # Implementation would use matplotlib text rendering
        
        return frame