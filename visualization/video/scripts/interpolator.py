"""
Frame interpolation for smooth animations.
"""
import numpy as np
import pandas as pd
from typing import List, Dict, Tuple
from scipy.interpolate import interp1d
from scipy.ndimage import gaussian_filter


class FrameInterpolator:
    """Interpolate between snapshots for smooth animation."""
    
    def __init__(self, smoothing_factor: float = 0.5):
        self.smoothing_factor = smoothing_factor
    
    def interpolate_frames(self,
                          snapshots: List[pd.DataFrame],
                          timestamps: List[float],
                          target_fps: int = 60,
                          duration: float = 15.0) -> List[pd.DataFrame]:
        """
        Interpolate between snapshots for smooth animation.
        
        Args:
            snapshots: List of grid snapshots
            timestamps: Simulation times for each snapshot
            target_fps: Target frames per second
            duration: Video duration in seconds
            
        Returns:
            List of interpolated frames
        """
        total_frames = int(duration * target_fps)
        interpolated_frames = []
        
        # Create interpolation functions for continuous variables
        continuous_vars = ['moisture', 'temperature', 'elevation']
        interpolators = {}
        
        # Get grid dimensions
        first_snapshot = snapshots[0]
        width = first_snapshot.index.get_level_values('x').max() + 1
        height = first_snapshot.index.get_level_values('y').max() + 1
        
        # Prepare data for interpolation
        for var in continuous_vars:
            if var in first_snapshot.columns:
                # Create time series for each cell
                var_data = np.array([
                    snapshot[var].values for snapshot in snapshots
                ])
                
                # Create interpolator
                interpolators[var] = interp1d(
                    timestamps, var_data, axis=0, 
                    kind='linear', fill_value='extrapolate'
                )
        
        # Generate interpolated frames
        for frame_idx in range(total_frames):
            # Calculate simulation time for this frame
            video_time = frame_idx / target_fps
            sim_time = np.interp(
                video_time,
                [0, duration],
                [timestamps[0], timestamps[-1]]
            )
            
            # Create new frame
            frame_data = first_snapshot.copy()
            
            # Interpolate continuous variables
            for var, interp_func in interpolators.items():
                frame_data[var] = interp_func(sim_time)
            
            # Handle discrete states (fire spread)
            frame_data['state'] = self._interpolate_fire_states(
                snapshots, timestamps, sim_time, width, height
            )
            
            interpolated_frames.append(frame_data)
        
        return interpolated_frames
    
    def _interpolate_fire_states(self,
                                snapshots: List[pd.DataFrame],
                                timestamps: List[float],
                                target_time: float,
                                width: int,
                                height: int) -> pd.Series:
        """
        Interpolate discrete fire states with realistic transitions.
        """
        # Find surrounding snapshots
        idx_before = 0
        idx_after = len(timestamps) - 1
        
        for i in range(len(timestamps) - 1):
            if timestamps[i] <= target_time <= timestamps[i + 1]:
                idx_before = i
                idx_after = i + 1
                break
        
        # If same index, return that state
        if idx_before == idx_after:
            return snapshots[idx_before]['state'].copy()
        
        # Get states
        states_before = snapshots[idx_before]['state']
        states_after = snapshots[idx_after]['state']
        
        # Calculate interpolation factor
        t1, t2 = timestamps[idx_before], timestamps[idx_after]
        alpha = (target_time - t1) / (t2 - t1) if t2 != t1 else 0
        
        # Create transition probability map
        result_states = states_before.copy()
        
        # Find cells that transition
        tree_to_burning = (states_before == 'Tree') & (states_after == 'Burning')
        burning_to_burnt = (states_before == 'Burning') & (states_after == 'Burnt')
        
        # Apply transitions based on alpha with some randomness
        # This creates organic-looking fire spread
        if alpha > 0:
            # Trees catching fire
            transition_prob = self._create_fire_front_probability(
                states_before, states_after, width, height, alpha
            )
            
            tree_mask = tree_to_burning & (transition_prob > np.random.random(len(states_before)))
            result_states[tree_mask] = 'Burning'
            
            # Burning to burnt transition
            burnt_transition = alpha ** 2  # Slower transition for burning out
            burnt_mask = burning_to_burnt & (np.random.random(len(states_before)) < burnt_transition)
            result_states[burnt_mask] = 'Burnt'
        
        return result_states
    
    def _create_fire_front_probability(self,
                                      states_before: pd.Series,
                                      states_after: pd.Series,
                                      width: int,
                                      height: int,
                                      alpha: float) -> np.ndarray:
        """
        Create probability map for fire front progression.
        """
        # Convert states to grid
        grid_before = self._states_to_grid(states_before, width, height)
        grid_after = self._states_to_grid(states_after, width, height)
        
        # Find fire front (cells adjacent to burning cells)
        burning_before = (grid_before == 2).astype(float)
        
        # Apply Gaussian filter to create smooth fire front
        fire_influence = gaussian_filter(burning_before, sigma=1.5)
        
        # Create probability based on distance from fire and alpha
        probability = fire_influence * alpha * 2  # Scale factor for visible progression
        
        # Add some noise for organic look
        noise = np.random.normal(0, 0.1, probability.shape)
        probability += noise * alpha
        
        # Clip to valid range
        probability = np.clip(probability, 0, 1)
        
        return probability.flatten()
    
    def _states_to_grid(self, states: pd.Series, width: int, height: int) -> np.ndarray:
        """Convert state series to 2D grid."""
        state_map = {'Empty': 0, 'Tree': 1, 'Burning': 2, 'Burnt': 3}
        
        grid = np.zeros((height, width), dtype=int)
        for idx, state in states.items():
            if isinstance(idx, tuple):
                x, y = idx
            else:
                # Assume sequential ordering
                y = idx // width
                x = idx % width
            
            grid[y, x] = state_map.get(state, 0)
        
        return grid
    
    def smooth_trajectory(self,
                         positions: List[Tuple[float, float, float]],
                         timestamps: List[float],
                         target_times: List[float]) -> List[Tuple[float, float, float]]:
        """
        Smooth camera trajectory using spline interpolation.
        
        Args:
            positions: List of (x, y, z) camera positions
            timestamps: Time for each position
            target_times: Times to interpolate
            
        Returns:
            Smoothed positions
        """
        positions_array = np.array(positions)
        
        # Create interpolators for each dimension
        x_interp = interp1d(timestamps, positions_array[:, 0], 
                           kind='cubic', fill_value='extrapolate')
        y_interp = interp1d(timestamps, positions_array[:, 1],
                           kind='cubic', fill_value='extrapolate')
        z_interp = interp1d(timestamps, positions_array[:, 2],
                           kind='cubic', fill_value='extrapolate')
        
        # Interpolate positions
        smooth_positions = []
        for t in target_times:
            x = float(x_interp(t))
            y = float(y_interp(t))
            z = float(z_interp(t))
            smooth_positions.append((x, y, z))
        
        return smooth_positions