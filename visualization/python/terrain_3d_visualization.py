"""
3D terrain visualization for forest fire simulations.
Creates interactive 3D visualizations showing fire spread on topographic surfaces.
"""
import numpy as np
import pandas as pd
import plotly.graph_objects as go
import plotly.express as px
from plotly.subplots import make_subplots
import matplotlib.pyplot as plt
from matplotlib import cm
from typing import Dict, List, Optional, Tuple
import warnings

from utils.data_loader import (load_grid_snapshot, grid_to_matrix,
                              extract_state_matrix, infer_grid_dimensions,
                              load_all_snapshots)
from utils.color_schemes import CELL_STATE_COLORS, VEGETATION_COLORS
from utils.plot_config import save_figure


def create_3d_terrain_surface(grid_data: pd.DataFrame,
                            elevation_column: str = 'elevation',
                            color_by: str = 'state',
                            title: str = 'Forest Fire on 3D Terrain',
                            camera_position: Optional[Dict] = None,
                            show_contours: bool = True,
                            output_name: Optional[str] = None) -> go.Figure:
    """
    Create interactive 3D terrain visualization with fire state overlay.
    
    Args:
        grid_data: DataFrame with grid data including elevation
        elevation_column: Column name for elevation data
        color_by: Column to use for coloring ('state', 'vegetation', 'temperature')
        title: Plot title
        camera_position: Dict with camera position settings
        show_contours: Whether to show elevation contours
        output_name: Filename for saving
        
    Returns:
        Plotly figure object
    """
    # Check if elevation data exists
    if elevation_column not in grid_data.columns:
        warnings.warn(f"No elevation data found in column '{elevation_column}'")
        return None
    
    # Get grid dimensions
    width, height = infer_grid_dimensions(grid_data)
    
    # Create elevation matrix
    elevation_matrix = grid_to_matrix(grid_data, elevation_column)
    
    # Create color matrix based on specified column
    if color_by == 'state':
        # Create custom colorscale for cell states
        color_matrix = extract_state_matrix(grid_data)
        colorscale = [
            [0.0, CELL_STATE_COLORS['Empty']],
            [0.25, CELL_STATE_COLORS['Tree']],
            [0.5, CELL_STATE_COLORS['Burning']],
            [0.75, CELL_STATE_COLORS['Burnt']],
            [1.0, CELL_STATE_COLORS['Burnt']]
        ]
        colorbar_title = "Cell State"
        
        # Create discrete colorbar tick labels
        colorbar_tickvals = [0, 1, 2, 3]
        colorbar_ticktext = ['Empty', 'Tree', 'Burning', 'Burnt']
        
    elif color_by == 'vegetation' and 'vegetation' in grid_data.columns:
        # Map vegetation types to numeric values
        veg_types = grid_data['vegetation'].unique()
        veg_map = {veg: i for i, veg in enumerate(veg_types)}
        
        color_matrix = np.zeros((height, width))
        for idx, row in grid_data.iterrows():
            if isinstance(idx, tuple):
                x, y = idx
            else:
                x, y = int(row['x']), int(row['y'])
            color_matrix[y, x] = veg_map[row['vegetation']]
        
        # Create vegetation colorscale
        n_veg_types = len(veg_types)
        colorscale = []
        for i, veg in enumerate(veg_types):
            color = VEGETATION_COLORS.get(veg, '#808080')
            colorscale.append([i/n_veg_types, color])
            colorscale.append([(i+1)/n_veg_types, color])
        
        colorbar_title = "Vegetation Type"
        colorbar_tickvals = list(range(len(veg_types)))
        colorbar_ticktext = list(veg_types)
        
    else:
        # Use continuous data
        if color_by in grid_data.columns:
            color_matrix = grid_to_matrix(grid_data, color_by)
            colorscale = 'Viridis'
            colorbar_title = color_by.replace('_', ' ').title()
            colorbar_tickvals = None
            colorbar_ticktext = None
        else:
            # Fallback to elevation
            color_matrix = elevation_matrix
            colorscale = 'earth'
            colorbar_title = "Elevation (m)"
            colorbar_tickvals = None
            colorbar_ticktext = None
    
    # Create 3D surface plot
    fig = go.Figure(data=[
        go.Surface(
            z=elevation_matrix,
            surfacecolor=color_matrix,
            colorscale=colorscale,
            showscale=True,
            colorbar=dict(
                title=colorbar_title,
                tickvals=colorbar_tickvals,
                ticktext=colorbar_ticktext,
                len=0.7,
                x=1.02
            ),
            contours=dict(
                z=dict(
                    show=show_contours,
                    usecolormap=False,
                    highlightcolor="gray",
                    project=dict(z=True)
                )
            ) if show_contours else None,
            hovertemplate='X: %{x}<br>Y: %{y}<br>Elevation: %{z:.0f}m<br>' +
                         f'{colorbar_title}: %{{surfacecolor:.0f}}<extra></extra>'
        )
    ])
    
    # Update layout
    if camera_position is None:
        camera_position = dict(
            eye=dict(x=1.5, y=1.5, z=1.2),
            up=dict(x=0, y=0, z=1),
            center=dict(x=0, y=0, z=0)
        )
    
    fig.update_layout(
        title=dict(text=title, font=dict(size=16)),
        scene=dict(
            xaxis_title="X (grid units)",
            yaxis_title="Y (grid units)",
            zaxis_title="Elevation (m)",
            camera=camera_position,
            aspectmode='manual',
            aspectratio=dict(x=1, y=height/width, z=0.5)
        ),
        width=900,
        height=700,
        margin=dict(l=0, r=0, t=40, b=0)
    )
    
    # Save if requested
    if output_name:
        # Save as HTML for interactivity
        html_path = output_name.replace('.png', '.html').replace('.pdf', '.html')
        fig.write_html(html_path)
        print(f"Saved interactive plot to: {html_path}")
        
        # Also save static image
        try:
            fig.write_image(f"{output_name}.png", width=1200, height=900, scale=2)
            print(f"Saved static image to: {output_name}.png")
        except:
            print("Note: Install kaleido package for static image export")
    
    return fig


def create_3d_animation(snapshots: Dict[str, pd.DataFrame],
                       elevation_column: str = 'elevation',
                       fps: int = 5,
                       output_name: Optional[str] = None) -> go.Figure:
    """
    Create animated 3D visualization of fire spread over time.
    
    Args:
        snapshots: Dict of timestamp -> grid DataFrame
        elevation_column: Column name for elevation data
        fps: Frames per second for animation
        output_name: Filename for saving
        
    Returns:
        Plotly figure object with animation
    """
    # Sort snapshots by time
    sorted_times = sorted(snapshots.keys())
    
    # Check if elevation exists
    first_snapshot = snapshots[sorted_times[0]]
    if elevation_column not in first_snapshot.columns:
        warnings.warn("No elevation data found for 3D animation")
        return None
    
    # Get dimensions from first snapshot
    width, height = infer_grid_dimensions(first_snapshot)
    
    # Create frames
    frames = []
    
    for time_key in sorted_times:
        grid_data = snapshots[time_key]
        
        # Extract matrices
        elevation_matrix = grid_to_matrix(grid_data, elevation_column)
        state_matrix = extract_state_matrix(grid_data)
        
        # Create frame
        frame = go.Frame(
            data=[go.Surface(
                z=elevation_matrix,
                surfacecolor=state_matrix,
                showscale=False
            )],
            name=str(time_key),
            traces=[0]
        )
        frames.append(frame)
    
    # Create initial figure
    initial_elevation = grid_to_matrix(snapshots[sorted_times[0]], elevation_column)
    initial_state = extract_state_matrix(snapshots[sorted_times[0]])
    
    fig = go.Figure(
        data=[go.Surface(
            z=initial_elevation,
            surfacecolor=initial_state,
            colorscale=[
                [0.0, CELL_STATE_COLORS['Empty']],
                [0.25, CELL_STATE_COLORS['Tree']],
                [0.5, CELL_STATE_COLORS['Burning']],
                [0.75, CELL_STATE_COLORS['Burnt']],
                [1.0, CELL_STATE_COLORS['Burnt']]
            ],
            showscale=True,
            colorbar=dict(
                title="Cell State",
                tickvals=[0, 1, 2, 3],
                ticktext=['Empty', 'Tree', 'Burning', 'Burnt'],
                len=0.7,
                x=1.02
            )
        )],
        frames=frames
    )
    
    # Add animation controls
    fig.update_layout(
        title="Fire Spread Animation on 3D Terrain",
        updatemenus=[{
            'type': 'buttons',
            'showactive': False,
            'y': 0,
            'x': 0.1,
            'xanchor': 'right',
            'yanchor': 'top',
            'buttons': [
                {
                    'label': 'Play',
                    'method': 'animate',
                    'args': [None, {
                        'frame': {'duration': 1000/fps, 'redraw': True},
                        'fromcurrent': True,
                        'mode': 'immediate'
                    }]
                },
                {
                    'label': 'Pause',
                    'method': 'animate',
                    'args': [[None], {
                        'frame': {'duration': 0, 'redraw': False},
                        'mode': 'immediate'
                    }]
                }
            ]
        }],
        sliders=[{
            'active': 0,
            'steps': [
                {
                    'label': str(time_key),
                    'method': 'animate',
                    'args': [[str(time_key)], {
                        'frame': {'duration': 0, 'redraw': True},
                        'mode': 'immediate'
                    }]
                } for time_key in sorted_times
            ],
            'x': 0.1,
            'len': 0.9,
            'xanchor': 'left',
            'y': 0,
            'yanchor': 'top'
        }],
        scene=dict(
            xaxis_title="X (grid units)",
            yaxis_title="Y (grid units)",
            zaxis_title="Elevation (m)",
            camera=dict(
                eye=dict(x=1.5, y=1.5, z=1.2),
                up=dict(x=0, y=0, z=1)
            ),
            aspectmode='manual',
            aspectratio=dict(x=1, y=height/width, z=0.5)
        ),
        width=900,
        height=700
    )
    
    if output_name:
        html_path = output_name.replace('.png', '_animation.html')
        fig.write_html(html_path)
        print(f"Saved animation to: {html_path}")
    
    return fig


def create_cross_section_view(grid_data: pd.DataFrame,
                            section_type: str = 'x',
                            section_index: int = None,
                            output_name: Optional[str] = None) -> plt.Figure:
    """
    Create 2D cross-section view of terrain with fire states.
    
    Args:
        grid_data: DataFrame with grid data
        section_type: 'x' for vertical section, 'y' for horizontal
        section_index: Index of the section (if None, uses middle)
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    # Get dimensions
    width, height = infer_grid_dimensions(grid_data)
    
    # Determine section index
    if section_index is None:
        section_index = width // 2 if section_type == 'x' else height // 2
    
    # Extract data for cross-section
    if section_type == 'x':
        # Vertical cross-section (constant x)
        section_data = grid_data[grid_data.index.get_level_values('x') == section_index]
        position_col = 'y'
        position_label = 'Y Position'
    else:
        # Horizontal cross-section (constant y)
        section_data = grid_data[grid_data.index.get_level_values('y') == section_index]
        position_col = 'x'
        position_label = 'X Position'
    
    if section_data.empty:
        warnings.warn("No data found for specified cross-section")
        return None
    
    # Sort by position
    section_data = section_data.sort_index()
    
    # Create figure
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8), height_ratios=[2, 1],
                                   sharex=True)
    
    # Extract data
    positions = section_data.index.get_level_values(position_col).values
    elevations = section_data['elevation'].values if 'elevation' in section_data.columns else np.zeros_like(positions)
    states = section_data['state'].values if 'state' in section_data.columns else ['Empty'] * len(positions)
    
    # Top panel: Elevation profile with state colors
    ax1.fill_between(positions, 0, elevations, color='lightgray', alpha=0.5)
    ax1.plot(positions, elevations, 'k-', linewidth=2)
    
    # Color points by state
    for i, (pos, elev, state) in enumerate(zip(positions, elevations, states)):
        color = CELL_STATE_COLORS.get(state, '#808080')
        ax1.scatter(pos, elev, c=color, s=50, edgecolor='black', linewidth=1, zorder=5)
    
    ax1.set_ylabel('Elevation (m)')
    ax1.set_title(f'Terrain Cross-Section ({section_type.upper()}={section_index})')
    ax1.grid(True, alpha=0.3)
    
    # Add legend
    from matplotlib.patches import Patch
    legend_elements = [Patch(facecolor=CELL_STATE_COLORS[state], label=state)
                      for state in ['Empty', 'Tree', 'Burning', 'Burnt']]
    ax1.legend(handles=legend_elements, loc='upper right')
    
    # Bottom panel: Additional data (moisture, temperature, etc.)
    if 'moisture' in section_data.columns:
        moisture = section_data['moisture'].values
        ax2.plot(positions, moisture, 'b-', label='Moisture', linewidth=2)
        ax2.set_ylabel('Moisture', color='blue')
        ax2.tick_params(axis='y', labelcolor='blue')
        
        if 'temperature' in section_data.columns:
            ax2_twin = ax2.twinx()
            temperature = section_data['temperature'].values
            ax2_twin.plot(positions, temperature, 'r-', label='Temperature', linewidth=2)
            ax2_twin.set_ylabel('Temperature (°C)', color='red')
            ax2_twin.tick_params(axis='y', labelcolor='red')
    
    ax2.set_xlabel(position_label)
    ax2.grid(True, alpha=0.3)
    
    plt.tight_layout()
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def create_multi_view_3d(grid_data: pd.DataFrame,
                        output_name: Optional[str] = None) -> go.Figure:
    """
    Create multiple 3D views of the terrain from different angles.
    
    Args:
        grid_data: DataFrame with grid data
        output_name: Filename for saving
        
    Returns:
        Plotly figure object
    """
    # Define camera positions
    camera_positions = {
        'Top View': dict(eye=dict(x=0, y=0, z=2.5)),
        'Front View': dict(eye=dict(x=0, y=-2, z=0.5)),
        'Side View': dict(eye=dict(x=2, y=0, z=0.5)),
        'Perspective': dict(eye=dict(x=1.5, y=-1.5, z=1.2))
    }
    
    # Create subplots
    fig = make_subplots(
        rows=2, cols=2,
        specs=[[{'type': 'surface'}, {'type': 'surface'}],
               [{'type': 'surface'}, {'type': 'surface'}]],
        subplot_titles=list(camera_positions.keys()),
        vertical_spacing=0.1,
        horizontal_spacing=0.1
    )
    
    # Get data
    elevation_matrix = grid_to_matrix(grid_data, 'elevation')
    state_matrix = extract_state_matrix(grid_data)
    
    # Add surface to each subplot
    for i, (view_name, camera) in enumerate(camera_positions.items()):
        row = i // 2 + 1
        col = i % 2 + 1
        
        fig.add_trace(
            go.Surface(
                z=elevation_matrix,
                surfacecolor=state_matrix,
                colorscale=[
                    [0.0, CELL_STATE_COLORS['Empty']],
                    [0.25, CELL_STATE_COLORS['Tree']],
                    [0.5, CELL_STATE_COLORS['Burning']],
                    [0.75, CELL_STATE_COLORS['Burnt']],
                    [1.0, CELL_STATE_COLORS['Burnt']]
                ],
                showscale=(i == 0),  # Only show colorbar for first subplot
                colorbar=dict(
                    title="State",
                    tickvals=[0, 1, 2, 3],
                    ticktext=['Empty', 'Tree', 'Fire', 'Burnt'],
                    len=0.5,
                    x=1.1
                ) if i == 0 else None
            ),
            row=row, col=col
        )
        
        # Update scene for this subplot
        scene_name = f'scene{i+1}' if i > 0 else 'scene'
        fig.update_layout({
            scene_name: dict(
                camera=camera,
                xaxis_title="X",
                yaxis_title="Y",
                zaxis_title="Z (m)",
                aspectmode='manual',
                aspectratio=dict(x=1, y=1, z=0.5)
            )
        })
    
    fig.update_layout(
        title="Multi-View 3D Terrain Analysis",
        height=800,
        width=1000
    )
    
    if output_name:
        html_path = output_name.replace('.png', '_multiview.html')
        fig.write_html(html_path)
        print(f"Saved multi-view to: {html_path}")
    
    return fig


def create_3d_summary(grid_snapshots: Dict[str, pd.DataFrame],
                     output_dir: str = "visualization/figures"):
    """
    Create comprehensive 3D visualizations from grid snapshots.
    
    Args:
        grid_snapshots: Dict mapping names to grid DataFrames
        output_dir: Output directory for figures
    """
    for name, grid_data in grid_snapshots.items():
        print(f"Processing 3D visualization for {name}...")
        
        # Basic 3D terrain with fire state
        fig = create_3d_terrain_surface(grid_data,
                                      title=f'3D Terrain: {name}',
                                      output_name=f'terrain_3d_{name}')
        
        # Cross-sections
        if 'elevation' in grid_data.columns:
            # X cross-section
            fig_x = create_cross_section_view(grid_data, section_type='x',
                                            output_name=f'cross_section_x_{name}')
            if fig_x:
                plt.close(fig_x)
            
            # Y cross-section
            fig_y = create_cross_section_view(grid_data, section_type='y',
                                            output_name=f'cross_section_y_{name}')
            if fig_y:
                plt.close(fig_y)
        
        # Multi-view if elevation exists
        if 'elevation' in grid_data.columns:
            fig_multi = create_multi_view_3d(grid_data,
                                           output_name=f'multiview_{name}')
    
    # Create animation if multiple snapshots
    if len(grid_snapshots) > 1:
        print("Creating 3D animation...")
        fig_anim = create_3d_animation(grid_snapshots,
                                     output_name='terrain_3d_animation')