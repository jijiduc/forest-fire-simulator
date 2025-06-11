#!/usr/bin/env python3
"""
Enhanced 3D terrain visualization for forest fire simulation
Generates realistic 3D views with clear fire state visualization
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.colors import LinearSegmentedColormap, ListedColormap
from matplotlib import cm
from pathlib import Path
import matplotlib.patches as mpatches
from scipy.ndimage import gaussian_filter
import warnings
warnings.filterwarnings('ignore')

class EnhancedTerrain3DRenderer:
    def __init__(self, elevation_data_path):
        """Initialize with elevation data"""
        self.elevation_df = pd.read_csv(elevation_data_path)
        self.setup_terrain_mesh()
        self.setup_camera_path()
        
    def setup_terrain_mesh(self):
        """Create terrain mesh from elevation data"""
        # Get grid dimensions
        self.width = self.elevation_df['x'].max() + 1
        self.height = self.elevation_df['y'].max() + 1
        
        # Create elevation grid
        self.elevation_grid = np.zeros((self.height, self.width))
        for _, row in self.elevation_df.iterrows():
            self.elevation_grid[int(row['y']), int(row['x'])] = row['elevation']
        
        # Normalize elevation for better visualization
        self.elev_min = self.elevation_grid.min()
        self.elev_max = self.elevation_grid.max()
        self.elev_range = self.elev_max - self.elev_min
        
        # Smooth elevation slightly for better visualization
        self.elevation_smooth = gaussian_filter(self.elevation_grid, sigma=0.5)
        
        # Create coordinate grids
        self.X, self.Y = np.meshgrid(range(self.width), range(self.height))
        
        # Scale elevation for better aspect ratio
        self.Z_scaled = (self.elevation_smooth - self.elev_min) * 0.3
        
    def setup_camera_path(self):
        """Define camera positions for animation"""
        self.camera_positions = []
        
        # Calculate based on grid size
        center_x = self.width / 2
        center_y = self.height / 2
        
        # Overview position
        self.camera_positions.append({
            'elev': 45,
            'azim': -45,
            'distance': 1.2
        })
        
        # Approach angle
        self.camera_positions.append({
            'elev': 35,
            'azim': -60,
            'distance': 1.0
        })
        
        # Dramatic low angle
        self.camera_positions.append({
            'elev': 25,
            'azim': -75,
            'distance': 0.9
        })
        
        # Side view
        self.camera_positions.append({
            'elev': 30,
            'azim': -90,
            'distance': 1.1
        })
        
    def interpolate_camera(self, pos1, pos2, t):
        """Smooth interpolation between camera positions"""
        return {
            'elev': pos1['elev'] * (1-t) + pos2['elev'] * t,
            'azim': pos1['azim'] * (1-t) + pos2['azim'] * t,
            'distance': pos1['distance'] * (1-t) + pos2['distance'] * t
        }
        
    def load_frame_data(self, frame_path):
        """Load cell states for a frame"""
        df = pd.read_csv(frame_path)
        
        # Create state grid
        state_grid = np.empty((self.height, self.width), dtype=object)
        state_grid.fill('empty')
        
        for _, cell in df.iterrows():
            x, y = int(cell['x']), int(cell['y'])
            state_grid[y, x] = cell['state']
            
        return state_grid
    
    def create_state_colors(self, state_grid):
        """Create color array based on cell states"""
        # Create RGBA color array
        colors = np.ones((self.height, self.width, 4))
        
        # Define state colors with proper contrast
        state_colors = {
            'empty': [0.95, 0.95, 0.95, 0.3],      # Very light gray, mostly transparent
            'tree': [0.1, 0.5, 0.1, 0.9],          # Forest green
            'burning_0.5': [1.0, 0.6, 0.0, 0.95],  # Orange fire
            'burnt': [0.3, 0.2, 0.15, 0.9]         # Dark brown
        }
        
        # Apply colors based on state
        for y in range(self.height):
            for x in range(self.width):
                state = state_grid[y, x]
                if state in state_colors:
                    colors[y, x] = state_colors[state]
                elif state.startswith('burning_'):
                    # Handle burning states with intensity
                    intensity = float(state.split('_')[1])
                    # Gradient from yellow to red based on intensity
                    r = 1.0
                    g = 0.8 - 0.6 * intensity
                    b = 0.0
                    a = 0.9 + 0.1 * intensity
                    colors[y, x] = [r, g, b, a]
                    
        return colors
    
    def add_terrain_shading(self, ax, alpha=0.3):
        """Add hillshade effect to terrain"""
        # Calculate gradient for shading
        dy, dx = np.gradient(self.elevation_smooth)
        slope = np.sqrt(dx*dx + dy*dy)
        
        # Light source from northwest
        light_angle = np.pi / 4
        aspect = np.arctan2(dy, dx)
        shading = np.cos(aspect - light_angle) * slope
        shading = (shading - shading.min()) / (shading.max() - shading.min())
        
        # Apply as grayscale underlay
        ax.plot_surface(self.X, self.Y, self.Z_scaled - 0.1,
                       facecolors=cm.gray(shading),
                       alpha=alpha,
                       rstride=1, cstride=1,
                       antialiased=True,
                       shade=False)
    
    def create_3d_frame(self, frame_data, frame_number, output_path, camera_progress=0.0):
        """Generate a single 3D visualization frame with realistic terrain"""
        
        # Create figure with dark background
        fig = plt.figure(figsize=(19.2, 10.8), dpi=100)
        fig.patch.set_facecolor('#0a0a1e')  # Dark blue background
        
        ax = fig.add_subplot(111, projection='3d')
        ax.set_facecolor('#0a0a1e')
        
        # Get camera position based on progress
        cam_idx = int(camera_progress * (len(self.camera_positions) - 1))
        cam_t = (camera_progress * (len(self.camera_positions) - 1)) % 1.0
        
        if cam_idx < len(self.camera_positions) - 1:
            camera = self.interpolate_camera(
                self.camera_positions[cam_idx],
                self.camera_positions[cam_idx + 1],
                cam_t
            )
        else:
            camera = self.camera_positions[-1]
        
        # Add terrain with hillshading
        self.add_terrain_shading(ax, alpha=0.2)
        
        # Get state colors
        colors = self.create_state_colors(frame_data)
        
        # Plot main surface with cell states
        surf = ax.plot_surface(
            self.X, self.Y, self.Z_scaled,
            facecolors=colors,
            rstride=1, cstride=1,
            antialiased=True,
            shade=True,
            alpha=0.95
        )
        
        # Add contour lines for elevation
        contour_levels = np.linspace(self.Z_scaled.min(), self.Z_scaled.max(), 10)
        ax.contour(self.X, self.Y, self.Z_scaled, 
                   levels=contour_levels,
                   colors='white',
                   alpha=0.2,
                   linewidths=0.5,
                   offset=self.Z_scaled.min() - 1)
        
        # Add fire glow effect for burning cells
        self.add_fire_glow(ax, frame_data)
        
        # Set viewing angle
        ax.view_init(elev=camera['elev'], azim=camera['azim'])
        ax.dist = 10 * camera['distance']
        
        # Set axis properties
        ax.set_xlim(0, self.width)
        ax.set_ylim(0, self.height)
        ax.set_zlim(self.Z_scaled.min() - 1, self.Z_scaled.max() + 10)
        
        # Minimize axis visibility
        ax.grid(False)
        ax.xaxis.pane.fill = False
        ax.yaxis.pane.fill = False
        ax.zaxis.pane.fill = False
        ax.xaxis.pane.set_edgecolor('none')
        ax.yaxis.pane.set_edgecolor('none')
        ax.zaxis.pane.set_edgecolor('none')
        
        # Set tick colors
        ax.tick_params(axis='x', colors='white', labelsize=8)
        ax.tick_params(axis='y', colors='white', labelsize=8)
        ax.tick_params(axis='z', colors='white', labelsize=8)
        
        # Add labels
        ax.set_xlabel('X (cells)', color='white', fontsize=10)
        ax.set_ylabel('Y (cells)', color='white', fontsize=10)
        ax.set_zlabel('Elevation (m)', color='white', fontsize=10)
        
        # Add title
        title = f"Forest Fire Simulation - 3D Terrain View\nFrame: {frame_number}"
        ax.text2D(0.5, 0.95, title, transform=ax.transAxes,
                 fontsize=24, weight='bold', ha='center', color='white',
                 bbox=dict(boxstyle='round,pad=0.5', facecolor='black', alpha=0.7))
        
        # Add legend
        self.add_legend(ax)
        
        # Add frame info
        self.add_frame_info(ax, frame_data, frame_number)
        
        # Save with tight layout
        plt.tight_layout()
        plt.savefig(str(output_path), facecolor='#0a0a1e', dpi=100, bbox_inches='tight')
        plt.close()
        
    def add_fire_glow(self, ax, frame_data):
        """Add glowing particles for burning cells"""
        burn_x, burn_y, burn_z, burn_intensity = [], [], [], []
        
        for y in range(self.height):
            for x in range(self.width):
                state = frame_data[y, x]
                if state.startswith('burning_'):
                    intensity = float(state.split('_')[1])
                    # Add multiple particles for each burning cell
                    for _ in range(int(5 * intensity)):
                        burn_x.append(x + np.random.normal(0, 0.3))
                        burn_y.append(y + np.random.normal(0, 0.3))
                        burn_z.append(self.Z_scaled[y, x] + np.random.uniform(0.5, 2.0))
                        burn_intensity.append(intensity)
        
        if burn_x:
            # Plot fire particles
            scatter = ax.scatter(burn_x, burn_y, burn_z,
                               c=burn_intensity,
                               cmap='hot',
                               s=30,
                               alpha=0.6,
                               marker='o',
                               edgecolors='none')
    
    def add_legend(self, ax):
        """Add legend for cell states"""
        legend_elements = [
            mpatches.Rectangle((0, 0), 1, 1, facecolor=[0.1, 0.5, 0.1, 0.9], label='Tree'),
            mpatches.Rectangle((0, 0), 1, 1, facecolor=[1.0, 0.6, 0.0, 0.95], label='Burning'),
            mpatches.Rectangle((0, 0), 1, 1, facecolor=[0.3, 0.2, 0.15, 0.9], label='Burnt'),
            mpatches.Rectangle((0, 0), 1, 1, facecolor=[0.95, 0.95, 0.95, 0.3], label='Empty')
        ]
        
        legend = ax.legend(handles=legend_elements, loc='upper left',
                          bbox_to_anchor=(0.02, 0.98),
                          facecolor='black', edgecolor='white',
                          fontsize=12, framealpha=0.7)
        
        for text in legend.get_texts():
            text.set_color('white')
    
    def add_frame_info(self, ax, frame_data, frame_number):
        """Add frame statistics"""
        # Count states
        unique, counts = np.unique(frame_data, return_counts=True)
        state_counts = dict(zip(unique, counts))
        
        trees = sum(v for k, v in state_counts.items() if k == 'tree')
        burning = sum(v for k, v in state_counts.items() if k.startswith('burning'))
        burnt = sum(v for k, v in state_counts.items() if k == 'burnt')
        
        info_text = f"Trees: {trees} | Burning: {burning} | Burnt: {burnt}"
        
        ax.text2D(0.98, 0.02, info_text, transform=ax.transAxes,
                 fontsize=14, ha='right', va='bottom', color='white',
                 bbox=dict(boxstyle='round,pad=0.5', facecolor='black', alpha=0.7))
    
    def render_frame(self, frame_num, input_dir, output_dir, total_frames):
        """Render a single frame with appropriate camera position"""
        frame_path = input_dir / f"frames/frame_{frame_num:06d}.csv"
        output_path = output_dir / f"3d_frame_{frame_num:06d}.png"
        
        if not frame_path.exists():
            print(f"Warning: Frame {frame_num} not found")
            return
            
        # Calculate camera progress (0 to 1 over all frames)
        camera_progress = frame_num / max(total_frames - 1, 1)
        
        # Load frame data
        frame_data = self.load_frame_data(frame_path)
        
        # Create 3D visualization
        self.create_3d_frame(frame_data, frame_num, output_path, camera_progress)
        print(f"Generated 3D frame {frame_num}")

def generate_3d_frames(input_dir, output_dir, num_frames=None):
    """Generate all 3D frames"""
    input_path = Path(input_dir)
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)
    
    # Load elevation data
    elevation_path = input_path / "metadata/elevation.csv"
    if not elevation_path.exists():
        print("Error: Elevation data not found")
        return
        
    renderer = EnhancedTerrain3DRenderer(elevation_path)
    
    # Find all frame files
    frame_files = sorted(input_path.glob("frames/frame_*.csv"))
    frame_numbers = [int(f.stem.split('_')[1]) for f in frame_files]
    
    if num_frames:
        frame_numbers = frame_numbers[:num_frames]
    
    print(f"Rendering {len(frame_numbers)} 3D frames")
    
    # Render each frame
    for i, frame_num in enumerate(frame_numbers):
        renderer.render_frame(frame_num, input_path, output_path, len(frame_numbers))
        
    print(f"Completed {len(frame_numbers)} 3D frames")

def main():
    import argparse
    parser = argparse.ArgumentParser(description='Generate enhanced 3D terrain visualization frames')
    parser.add_argument('input_dir', help='Input directory with CSV exports')
    parser.add_argument('output_dir', help='Output directory for 3D frames')
    parser.add_argument('--frames', type=int, help='Number of frames to render')
    
    args = parser.parse_args()
    generate_3d_frames(args.input_dir, args.output_dir, args.frames)

if __name__ == '__main__':
    main()