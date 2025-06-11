"""
Frame generation module for video creation.
Renders individual frames with 2D grids, 3D terrain, and overlays.
"""
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.colors as mcolors
from matplotlib.figure import Figure
from matplotlib.backends.backend_agg import FigureCanvasAgg
from pathlib import Path
from typing import Dict, Tuple, Optional, List
import json

# Import color schemes from visualization module
import sys
sys.path.append(str(Path(__file__).parent.parent.parent / 'python'))
from utils.color_schemes import CELL_STATE_COLORS, create_state_colormap
from utils.plot_config import set_publication_style


class GridFrameRenderer:
    """Render 2D grid frames for video."""
    
    def __init__(self, resolution: Tuple[int, int] = (1920, 1080), dpi: int = 150):
        self.resolution = resolution
        self.dpi = dpi
        self.fig_size = (resolution[0] / dpi, resolution[1] / dpi)
        set_publication_style()
    
    def render_frame(self,
                    grid_data: pd.DataFrame,
                    frame_number: int,
                    layout: str = 'full',
                    show_grid: bool = False) -> np.ndarray:
        """
        Render a single 2D grid frame.
        
        Args:
            grid_data: Grid data DataFrame
            frame_number: Frame index
            layout: Layout type ('full', 'left', 'right')
            show_grid: Whether to show grid lines
            
        Returns:
            Frame as numpy array (RGB)
        """
        # Create figure
        fig = Figure(figsize=self.fig_size, dpi=self.dpi)
        canvas = FigureCanvasAgg(fig)
        
        # Determine layout
        if layout == 'full':
            ax = fig.add_subplot(111)
            ax.set_position([0.05, 0.05, 0.9, 0.9])
        elif layout == 'left':
            ax = fig.add_subplot(121)
            ax.set_position([0.025, 0.05, 0.45, 0.9])
        elif layout == 'right':
            ax = fig.add_subplot(122)
            ax.set_position([0.525, 0.05, 0.45, 0.9])
        else:
            ax = fig.add_subplot(111)
        
        # Extract grid dimensions
        width = grid_data.index.get_level_values('x').max() + 1
        height = grid_data.index.get_level_values('y').max() + 1
        
        # Create state matrix
        state_matrix = self._create_state_matrix(grid_data, width, height)
        
        # Create colormap
        cmap, norm = create_state_colormap()
        
        # Plot grid
        im = ax.imshow(state_matrix, cmap=cmap, norm=norm,
                      interpolation='nearest', aspect='equal')
        
        # Add grid lines if requested
        if show_grid and width <= 100:
            ax.set_xticks(np.arange(-0.5, width, 1), minor=True)
            ax.set_yticks(np.arange(-0.5, height, 1), minor=True)
            ax.grid(which="minor", color="gray", linestyle='-', 
                   linewidth=0.5, alpha=0.3)
        
        # Remove axes for cleaner look
        ax.set_xticks([])
        ax.set_yticks([])
        ax.set_xlim(-0.5, width - 0.5)
        ax.set_ylim(height - 0.5, -0.5)
        
        # Draw to canvas
        canvas.draw()
        
        # Convert to numpy array
        buf = canvas.buffer_rgba()
        frame = np.asarray(buf)[:, :, :3]  # Remove alpha channel
        
        plt.close(fig)
        
        return frame
    
    def _create_state_matrix(self, grid_data: pd.DataFrame, 
                           width: int, height: int) -> np.ndarray:
        """Convert grid data to state matrix."""
        state_map = {'Empty': 0, 'Tree': 1, 'Burning': 2, 'Burnt': 3}
        matrix = np.zeros((height, width))
        
        for idx, row in grid_data.iterrows():
            if isinstance(idx, tuple):
                x, y = idx
            else:
                x, y = int(row['x']), int(row['y'])
            
            state = row.get('state', 'Empty')
            matrix[int(y), int(x)] = state_map.get(state, 0)
        
        return matrix


class TerrainFrameRenderer:
    """Render 3D terrain frames for video."""
    
    def __init__(self, resolution: Tuple[int, int] = (1920, 1080), dpi: int = 150):
        self.resolution = resolution
        self.dpi = dpi
        self.fig_size = (resolution[0] / dpi, resolution[1] / dpi)
    
    def render_3d_frame(self,
                       grid_data: pd.DataFrame,
                       camera_position: Dict,
                       lighting_config: Optional[Dict] = None) -> np.ndarray:
        """
        Render 3D terrain frame.
        
        Args:
            grid_data: Grid data with elevation
            camera_position: Camera position dict with azimuth, elevation, distance
            lighting_config: Lighting configuration
            
        Returns:
            Frame as numpy array
        """
        from mpl_toolkits.mplot3d import Axes3D
        
        # Create figure
        fig = Figure(figsize=self.fig_size, dpi=self.dpi)
        canvas = FigureCanvasAgg(fig)
        ax = fig.add_subplot(111, projection='3d')
        
        # Extract data
        width = grid_data.index.get_level_values('x').max() + 1
        height = grid_data.index.get_level_values('y').max() + 1
        
        # Create meshgrid
        x = np.arange(width)
        y = np.arange(height)
        X, Y = np.meshgrid(x, y)
        
        # Get elevation data
        Z = self._create_elevation_matrix(grid_data, width, height)
        
        # Get state colors
        colors = self._create_color_matrix(grid_data, width, height)
        
        # Plot surface
        surf = ax.plot_surface(X, Y, Z, facecolors=colors,
                              linewidth=0, antialiased=True,
                              shade=True, alpha=0.95)
        
        # Set camera position
        ax.view_init(elev=camera_position.get('elevation', 30),
                    azim=camera_position.get('azimuth', -45))
        
        # Set distance (zoom)
        ax.dist = camera_position.get('distance', 10)
        
        # Configure axes
        ax.set_xlim(0, width-1)
        ax.set_ylim(0, height-1)
        ax.set_zlim(Z.min() * 0.9, Z.max() * 1.1)
        
        # Remove axes for cleaner look
        ax.set_axis_off()
        
        # Add lighting effects
        if lighting_config:
            ax.set_box_aspect([1, height/width, 0.3])
        
        # Draw to canvas
        canvas.draw()
        
        # Convert to numpy array
        buf = canvas.buffer_rgba()
        frame = np.asarray(buf)[:, :, :3]
        
        plt.close(fig)
        
        return frame
    
    def _create_elevation_matrix(self, grid_data: pd.DataFrame,
                                width: int, height: int) -> np.ndarray:
        """Create elevation matrix from grid data."""
        matrix = np.zeros((height, width))
        
        for idx, row in grid_data.iterrows():
            if isinstance(idx, tuple):
                x, y = idx
            else:
                x, y = int(row['x']), int(row['y'])
            
            elevation = row.get('elevation', 0)
            matrix[int(y), int(x)] = elevation
        
        return matrix
    
    def _create_color_matrix(self, grid_data: pd.DataFrame,
                            width: int, height: int) -> np.ndarray:
        """Create color matrix based on cell states."""
        colors = np.ones((height, width, 4))  # RGBA
        
        for idx, row in grid_data.iterrows():
            if isinstance(idx, tuple):
                x, y = idx
            else:
                x, y = int(row['x']), int(row['y'])
            
            state = row.get('state', 'Empty')
            color = CELL_STATE_COLORS.get(state, '#808080')
            
            # Convert hex to RGB
            rgb = mcolors.to_rgb(color)
            colors[int(y), int(x), :3] = rgb
        
        return colors


class OverlayRenderer:
    """Add overlays to frames."""
    
    def __init__(self):
        self.font_config = {
            'family': 'sans-serif',
            'weight': 'bold',
            'size': 14
        }
    
    def add_overlays(self,
                    frame: np.ndarray,
                    metrics: Dict,
                    time_info: Dict,
                    style: str = 'minimal') -> np.ndarray:
        """
        Add information overlays to frames.
        
        Args:
            frame: Frame array
            metrics: Simulation metrics
            time_info: Time information
            style: Overlay style
            
        Returns:
            Frame with overlays
        """
        # Create figure for overlay
        height, width = frame.shape[:2]
        fig = Figure(figsize=(width/100, height/100), dpi=100)
        canvas = FigureCanvasAgg(fig)
        ax = fig.add_subplot(111)
        
        # Display frame
        ax.imshow(frame)
        ax.set_xlim(0, width)
        ax.set_ylim(height, 0)
        ax.axis('off')
        
        # Add time counter
        time_text = f"Time: {time_info['video_time']:.1f}s"
        ax.text(width - 10, 30, time_text,
               ha='right', va='top',
               fontdict=self.font_config,
               bbox=dict(boxstyle='round,pad=0.5', 
                        facecolor='black', alpha=0.7),
               color='white')
        
        # Add metrics based on style
        if style in ['minimal', 'full']:
            metric_texts = []
            
            if 'burnt_fraction' in metrics:
                metric_texts.append(f"Burnt: {metrics['burnt_fraction']:.1%}")
            
            if 'active_fires' in metrics:
                metric_texts.append(f"Active: {metrics['active_fires']}")
            
            if 'percolation' in metrics:
                perc_value = metrics['percolation']
                metric_texts.append(f"Percolation: {perc_value:.2f}")
            
            # Display metrics
            if metric_texts:
                metrics_text = '\n'.join(metric_texts)
                ax.text(10, 30, metrics_text,
                       ha='left', va='top',
                       fontdict=self.font_config,
                       bbox=dict(boxstyle='round,pad=0.5',
                                facecolor='black', alpha=0.7),
                       color='white')
        
        # Add scenario label if present
        if 'scenario' in time_info:
            scenario_text = time_info['scenario'].upper()
            ax.text(width/2, height - 30, scenario_text,
                   ha='center', va='bottom',
                   fontdict={'size': 18, 'weight': 'bold'},
                   bbox=dict(boxstyle='round,pad=0.5',
                            facecolor='darkred', alpha=0.8),
                   color='white')
        
        # Draw to canvas
        canvas.draw()
        
        # Convert to numpy array
        buf = canvas.buffer_rgba()
        overlay_frame = np.asarray(buf)[:, :, :3]
        
        plt.close(fig)
        
        return overlay_frame
    
    def add_legend(self, frame: np.ndarray, position: str = 'bottom-right') -> np.ndarray:
        """Add legend to frame."""
        height, width = frame.shape[:2]
        fig = Figure(figsize=(width/100, height/100), dpi=100)
        canvas = FigureCanvasAgg(fig)
        ax = fig.add_subplot(111)
        
        # Display frame
        ax.imshow(frame)
        ax.set_xlim(0, width)
        ax.set_ylim(height, 0)
        ax.axis('off')
        
        # Create legend elements
        legend_elements = [
            mpatches.Patch(facecolor=CELL_STATE_COLORS['Tree'], 
                          label='Forest', edgecolor='black'),
            mpatches.Patch(facecolor=CELL_STATE_COLORS['Burning'],
                          label='Fire', edgecolor='black'),
            mpatches.Patch(facecolor=CELL_STATE_COLORS['Burnt'],
                          label='Burnt', edgecolor='black')
        ]
        
        # Position legend
        if position == 'bottom-right':
            loc = 'lower right'
            bbox_to_anchor = (0.98, 0.02)
        elif position == 'top-right':
            loc = 'upper right'
            bbox_to_anchor = (0.98, 0.98)
        else:
            loc = 'best'
            bbox_to_anchor = None
        
        # Add legend
        ax.legend(handles=legend_elements, loc=loc,
                 bbox_to_anchor=bbox_to_anchor,
                 frameon=True, fancybox=True,
                 fontsize=12, framealpha=0.9,
                 facecolor='white', edgecolor='black')
        
        # Draw and convert
        canvas.draw()
        buf = canvas.buffer_rgba()
        frame_with_legend = np.asarray(buf)[:, :, :3]
        
        plt.close(fig)
        
        return frame_with_legend