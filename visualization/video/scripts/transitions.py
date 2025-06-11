"""
Transition effects for video segments.
"""
import numpy as np
from typing import List, Tuple, Optional
from scipy.ndimage import gaussian_filter


class TransitionEffects:
    """Create smooth transitions between video segments."""
    
    def cross_fade(self,
                   frame1: np.ndarray,
                   frame2: np.ndarray,
                   progress: float) -> np.ndarray:
        """
        Smooth cross-fade between scenes.
        
        Args:
            frame1: First frame
            frame2: Second frame
            progress: Transition progress (0-1)
            
        Returns:
            Blended frame
        """
        # Ensure frames have same shape
        assert frame1.shape == frame2.shape, "Frames must have same dimensions"
        
        # Linear interpolation
        alpha = progress
        blended = (1 - alpha) * frame1 + alpha * frame2
        
        return blended.astype(np.uint8)
    
    def split_screen_wipe(self,
                         frame1: np.ndarray,
                         frame2: np.ndarray,
                         progress: float,
                         direction: str = 'horizontal') -> np.ndarray:
        """
        Wipe transition for split-screen reveal.
        
        Args:
            frame1: First frame
            frame2: Second frame
            progress: Transition progress (0-1)
            direction: 'horizontal', 'vertical', 'diagonal'
            
        Returns:
            Transition frame
        """
        height, width = frame1.shape[:2]
        result = frame1.copy()
        
        if direction == 'horizontal':
            # Wipe from left to right
            split_x = int(width * progress)
            result[:, :split_x] = frame2[:, :split_x]
            
            # Add edge blur for smoother transition
            if 0 < split_x < width - 5:
                edge_region = 5
                for i in range(edge_region):
                    alpha = i / edge_region
                    x = split_x - edge_region // 2 + i
                    if 0 <= x < width:
                        result[:, x] = (1 - alpha) * frame1[:, x] + alpha * frame2[:, x]
        
        elif direction == 'vertical':
            # Wipe from top to bottom
            split_y = int(height * progress)
            result[:split_y, :] = frame2[:split_y, :]
            
            # Add edge blur
            if 0 < split_y < height - 5:
                edge_region = 5
                for i in range(edge_region):
                    alpha = i / edge_region
                    y = split_y - edge_region // 2 + i
                    if 0 <= y < height:
                        result[y, :] = (1 - alpha) * frame1[y, :] + alpha * frame2[y, :]
        
        elif direction == 'diagonal':
            # Diagonal wipe from top-left to bottom-right
            for y in range(height):
                for x in range(width):
                    if (x + y) / (width + height) < progress:
                        result[y, x] = frame2[y, x]
        
        return result.astype(np.uint8)
    
    def zoom_transition(self,
                       frames: List[np.ndarray],
                       zoom_center: Tuple[int, int],
                       zoom_factor: float = 2.0) -> List[np.ndarray]:
        """
        Zoom in/out transition centered on specific point.
        
        Args:
            frames: List of frames to apply zoom
            zoom_center: (x, y) center point for zoom
            zoom_factor: Maximum zoom level
            
        Returns:
            List of zoomed frames
        """
        zoomed_frames = []
        n_frames = len(frames)
        
        for i, frame in enumerate(frames):
            # Calculate zoom level for this frame
            progress = i / (n_frames - 1) if n_frames > 1 else 0
            
            # Smooth zoom curve (ease in-out)
            t = progress
            if t < 0.5:
                zoom_progress = 2 * t * t
            else:
                zoom_progress = 1 - 2 * (1 - t) * (1 - t)
            
            current_zoom = 1 + (zoom_factor - 1) * zoom_progress
            
            # Apply zoom
            zoomed = self._apply_zoom(frame, zoom_center, current_zoom)
            zoomed_frames.append(zoomed)
        
        return zoomed_frames
    
    def _apply_zoom(self,
                   frame: np.ndarray,
                   center: Tuple[int, int],
                   zoom: float) -> np.ndarray:
        """Apply zoom transformation to frame."""
        height, width = frame.shape[:2]
        cx, cy = center
        
        # Calculate crop region
        crop_width = int(width / zoom)
        crop_height = int(height / zoom)
        
        x1 = max(0, cx - crop_width // 2)
        y1 = max(0, cy - crop_height // 2)
        x2 = min(width, x1 + crop_width)
        y2 = min(height, y1 + crop_height)
        
        # Crop and resize
        cropped = frame[y1:y2, x1:x2]
        
        # Resize back to original dimensions
        from scipy.ndimage import zoom as scipy_zoom
        zoom_y = height / cropped.shape[0]
        zoom_x = width / cropped.shape[1]
        
        zoomed = scipy_zoom(cropped, (zoom_y, zoom_x, 1), order=1)
        
        # Ensure correct shape
        if zoomed.shape[0] > height:
            zoomed = zoomed[:height]
        if zoomed.shape[1] > width:
            zoomed = zoomed[:, :width]
        
        return zoomed.astype(np.uint8)
    
    def fade_to_black(self,
                     frame: np.ndarray,
                     progress: float) -> np.ndarray:
        """Fade frame to black."""
        faded = frame * (1 - progress)
        return faded.astype(np.uint8)
    
    def fade_from_black(self,
                       frame: np.ndarray,
                       progress: float) -> np.ndarray:
        """Fade in from black."""
        faded = frame * progress
        return faded.astype(np.uint8)
    
    def apply_vignette(self,
                      frame: np.ndarray,
                      strength: float = 0.5) -> np.ndarray:
        """Apply vignette effect to frame."""
        height, width = frame.shape[:2]
        
        # Create radial gradient
        center_x, center_y = width // 2, height // 2
        Y, X = np.ogrid[:height, :width]
        
        # Calculate distance from center
        dist = np.sqrt((X - center_x)**2 + (Y - center_y)**2)
        max_dist = np.sqrt(center_x**2 + center_y**2)
        
        # Create vignette mask
        vignette = 1 - (dist / max_dist) * strength
        vignette = np.clip(vignette, 0, 1)
        
        # Apply vignette
        result = frame.copy()
        for i in range(3):  # Apply to each color channel
            result[:, :, i] = (frame[:, :, i] * vignette).astype(np.uint8)
        
        return result
    
    def create_split_screen(self,
                           frame1: np.ndarray,
                           frame2: np.ndarray,
                           split_type: str = 'vertical',
                           divider_width: int = 4) -> np.ndarray:
        """
        Create split screen composition.
        
        Args:
            frame1: Left/top frame
            frame2: Right/bottom frame
            split_type: 'vertical' or 'horizontal'
            divider_width: Width of divider line
            
        Returns:
            Split screen frame
        """
        height, width = frame1.shape[:2]
        
        if split_type == 'vertical':
            # Vertical split
            split_x = width // 2
            result = np.zeros_like(frame1)
            
            # Left side
            result[:, :split_x - divider_width//2] = frame1[:, :split_x - divider_width//2]
            
            # Right side
            result[:, split_x + divider_width//2:] = frame2[:, split_x + divider_width//2:]
            
            # Divider
            result[:, split_x - divider_width//2:split_x + divider_width//2] = 255
        
        elif split_type == 'horizontal':
            # Horizontal split
            split_y = height // 2
            result = np.zeros_like(frame1)
            
            # Top
            result[:split_y - divider_width//2, :] = frame1[:split_y - divider_width//2, :]
            
            # Bottom
            result[split_y + divider_width//2:, :] = frame2[split_y + divider_width//2:, :]
            
            # Divider
            result[split_y - divider_width//2:split_y + divider_width//2, :] = 255
        
        return result.astype(np.uint8)
    
    def apply_motion_blur(self,
                         frame: np.ndarray,
                         angle: float = 0,
                         strength: int = 10) -> np.ndarray:
        """Apply directional motion blur."""
        # Create motion blur kernel
        kernel_size = strength * 2 + 1
        kernel = np.zeros((kernel_size, kernel_size))
        
        # Calculate kernel values based on angle
        center = kernel_size // 2
        cos_angle = np.cos(np.radians(angle))
        sin_angle = np.sin(np.radians(angle))
        
        for i in range(kernel_size):
            x = int(center + (i - center) * cos_angle)
            y = int(center + (i - center) * sin_angle)
            if 0 <= x < kernel_size and 0 <= y < kernel_size:
                kernel[y, x] = 1
        
        kernel /= kernel.sum()
        
        # Apply blur
        from scipy.ndimage import convolve
        blurred = np.zeros_like(frame)
        
        for i in range(3):  # Apply to each channel
            blurred[:, :, i] = convolve(frame[:, :, i], kernel)
        
        return blurred.astype(np.uint8)