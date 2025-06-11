"""
Camera path definitions and control for 3D animations.
"""
import numpy as np
from typing import List, Dict, Tuple
from scipy.interpolate import interp1d
import math


class CameraPathController:
    """Generate and control camera movements for 3D views."""
    
    def __init__(self):
        self.paths = {
            'orbit': self._generate_orbit_path,
            'flyover': self._generate_flyover_path,
            'fire_follow': self._generate_fire_tracking_path,
            'dramatic_reveal': self._generate_reveal_path,
            'zoom_in': self._generate_zoom_path,
            'pan': self._generate_pan_path
        }
    
    def generate_camera_path(self,
                           key_points: List[Dict],
                           duration: float,
                           fps: int = 60,
                           smoothing: str = 'cubic') -> List[Dict]:
        """
        Generate smooth camera movements between keyframes.
        
        Args:
            key_points: List of camera keyframes with position and timing
            duration: Total duration in seconds
            fps: Frames per second
            smoothing: Interpolation method ('linear', 'cubic', 'quadratic')
            
        Returns:
            List of camera positions for each frame
        """
        total_frames = int(duration * fps)
        
        # Extract keyframe data
        times = [kp['time'] for kp in key_points]
        azimuths = [kp['azimuth'] for kp in key_points]
        elevations = [kp['elevation'] for kp in key_points]
        distances = [kp['distance'] for kp in key_points]
        
        # Create interpolation functions
        azimuth_interp = interp1d(times, azimuths, kind=smoothing, 
                                 fill_value='extrapolate')
        elevation_interp = interp1d(times, elevations, kind=smoothing,
                                   fill_value='extrapolate')
        distance_interp = interp1d(times, distances, kind=smoothing,
                                  fill_value='extrapolate')
        
        # Generate camera positions for each frame
        camera_positions = []
        for frame in range(total_frames):
            t = frame / fps
            
            camera_pos = {
                'frame': frame,
                'time': t,
                'azimuth': float(azimuth_interp(t)),
                'elevation': float(elevation_interp(t)),
                'distance': float(distance_interp(t))
            }
            
            camera_positions.append(camera_pos)
        
        return camera_positions
    
    def get_cinematic_paths(self, duration: float, fps: int = 60) -> Dict[str, List[Dict]]:
        """Get predefined cinematic camera movements."""
        return {
            name: func(duration, fps) 
            for name, func in self.paths.items()
        }
    
    def _generate_orbit_path(self, duration: float, fps: int) -> List[Dict]:
        """Generate circular orbit around the scene."""
        key_points = [
            {'time': 0.0, 'azimuth': -45, 'elevation': 30, 'distance': 10},
            {'time': duration * 0.25, 'azimuth': 45, 'elevation': 35, 'distance': 9},
            {'time': duration * 0.5, 'azimuth': 135, 'elevation': 40, 'distance': 8},
            {'time': duration * 0.75, 'azimuth': 225, 'elevation': 35, 'distance': 9},
            {'time': duration, 'azimuth': 315, 'elevation': 30, 'distance': 10}
        ]
        
        return self.generate_camera_path(key_points, duration, fps, 'cubic')
    
    def _generate_flyover_path(self, duration: float, fps: int) -> List[Dict]:
        """Generate flyover path from one corner to opposite."""
        key_points = [
            {'time': 0.0, 'azimuth': -45, 'elevation': 60, 'distance': 15},
            {'time': duration * 0.3, 'azimuth': 0, 'elevation': 80, 'distance': 12},
            {'time': duration * 0.7, 'azimuth': 90, 'elevation': 70, 'distance': 10},
            {'time': duration, 'azimuth': 135, 'elevation': 45, 'distance': 12}
        ]
        
        return self.generate_camera_path(key_points, duration, fps, 'cubic')
    
    def _generate_fire_tracking_path(self, duration: float, fps: int) -> List[Dict]:
        """Generate path that follows fire progression."""
        # This would ideally track actual fire data
        key_points = [
            {'time': 0.0, 'azimuth': -30, 'elevation': 25, 'distance': 8},
            {'time': duration * 0.3, 'azimuth': 0, 'elevation': 30, 'distance': 6},
            {'time': duration * 0.6, 'azimuth': 30, 'elevation': 35, 'distance': 5},
            {'time': duration, 'azimuth': 60, 'elevation': 40, 'distance': 7}
        ]
        
        return self.generate_camera_path(key_points, duration, fps, 'cubic')
    
    def _generate_reveal_path(self, duration: float, fps: int) -> List[Dict]:
        """Generate dramatic reveal starting from close-up."""
        key_points = [
            {'time': 0.0, 'azimuth': 0, 'elevation': 10, 'distance': 3},
            {'time': duration * 0.2, 'azimuth': -15, 'elevation': 20, 'distance': 5},
            {'time': duration * 0.5, 'azimuth': -30, 'elevation': 35, 'distance': 8},
            {'time': duration * 0.8, 'azimuth': -45, 'elevation': 45, 'distance': 10},
            {'time': duration, 'azimuth': -45, 'elevation': 50, 'distance': 12}
        ]
        
        return self.generate_camera_path(key_points, duration, fps, 'quadratic')
    
    def _generate_zoom_path(self, duration: float, fps: int) -> List[Dict]:
        """Generate zoom in/out path."""
        key_points = [
            {'time': 0.0, 'azimuth': -45, 'elevation': 30, 'distance': 12},
            {'time': duration * 0.5, 'azimuth': -45, 'elevation': 30, 'distance': 5},
            {'time': duration, 'azimuth': -45, 'elevation': 30, 'distance': 12}
        ]
        
        return self.generate_camera_path(key_points, duration, fps, 'cubic')
    
    def _generate_pan_path(self, duration: float, fps: int) -> List[Dict]:
        """Generate horizontal panning path."""
        key_points = [
            {'time': 0.0, 'azimuth': -90, 'elevation': 30, 'distance': 10},
            {'time': duration * 0.5, 'azimuth': 0, 'elevation': 30, 'distance': 10},
            {'time': duration, 'azimuth': 90, 'elevation': 30, 'distance': 10}
        ]
        
        return self.generate_camera_path(key_points, duration, fps, 'linear')
    
    def combine_paths(self,
                     paths: List[Tuple[str, float]],
                     total_duration: float,
                     fps: int = 60) -> List[Dict]:
        """
        Combine multiple camera paths sequentially.
        
        Args:
            paths: List of (path_name, duration) tuples
            total_duration: Total video duration
            fps: Frames per second
            
        Returns:
            Combined camera path
        """
        combined_path = []
        current_time = 0.0
        
        for path_name, path_duration in paths:
            if path_name in self.paths:
                # Generate path segment
                segment = self.paths[path_name](path_duration, fps)
                
                # Adjust timing and add to combined path
                for pos in segment:
                    adjusted_pos = pos.copy()
                    adjusted_pos['time'] += current_time
                    adjusted_pos['frame'] = int(adjusted_pos['time'] * fps)
                    combined_path.append(adjusted_pos)
                
                current_time += path_duration
        
        return combined_path
    
    def smooth_transition(self,
                         path1: List[Dict],
                         path2: List[Dict],
                         transition_duration: float,
                         fps: int = 60) -> List[Dict]:
        """
        Create smooth transition between two camera paths.
        
        Args:
            path1: First camera path
            path2: Second camera path
            transition_duration: Duration of transition
            fps: Frames per second
            
        Returns:
            Smoothly transitioned path
        """
        transition_frames = int(transition_duration * fps)
        
        # Get end of path1 and start of path2
        end_pos1 = path1[-1]
        start_pos2 = path2[0]
        
        # Create transition keyframes
        transition_keys = [
            {
                'time': 0,
                'azimuth': end_pos1['azimuth'],
                'elevation': end_pos1['elevation'],
                'distance': end_pos1['distance']
            },
            {
                'time': transition_duration,
                'azimuth': start_pos2['azimuth'],
                'elevation': start_pos2['elevation'],
                'distance': start_pos2['distance']
            }
        ]
        
        # Generate smooth transition
        transition = self.generate_camera_path(
            transition_keys, transition_duration, fps, 'cubic'
        )
        
        # Combine paths
        combined = path1[:-1] + transition + path2[1:]
        
        # Recalculate frame numbers
        for i, pos in enumerate(combined):
            pos['frame'] = i
            pos['time'] = i / fps
        
        return combined