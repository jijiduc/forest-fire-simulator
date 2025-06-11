"""
Spatial pattern visualization for forest fire simulations.
Creates grid visualizations, cluster analysis, and spatial correlations.
"""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.colors as mcolors
from matplotlib.animation import FuncAnimation, PillowWriter
from scipy.ndimage import label, distance_transform_edt
from scipy.spatial.distance import cdist
from scipy.stats import powerlaw
from sklearn.cluster import DBSCAN
from typing import Optional, List, Dict, Tuple
import warnings

from utils.data_loader import (load_grid_snapshot, infer_grid_dimensions,
                              extract_state_matrix, grid_to_matrix,
                              load_all_snapshots)
from utils.plot_config import (create_figure_with_subplots, save_figure,
                              format_axis_labels, add_colorbar, set_log_scale,
                              add_annotation)
from utils.color_schemes import (create_state_colormap, CELL_STATE_COLORS,
                                VEGETATION_COLORS, create_vegetation_colormap,
                                get_colorbar_label)


def plot_grid_state(grid_data: pd.DataFrame,
                   title: str = '',
                   highlight_clusters: bool = False,
                   show_metrics: bool = True,
                   show_colorbar: bool = True,
                   output_name: Optional[str] = None) -> plt.Figure:
    """
    Visualize grid state with optional cluster highlighting.
    
    Args:
        grid_data: DataFrame with grid data
        title: Plot title
        highlight_clusters: Whether to highlight fire clusters
        show_metrics: Whether to show grid metrics
        show_colorbar: Whether to show colorbar
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    # Extract state matrix
    state_matrix = extract_state_matrix(grid_data)
    width, height = state_matrix.shape[1], state_matrix.shape[0]
    
    # Create figure
    fig, ax = create_figure_with_subplots(fig_type='grid_view')
    
    # Create colormap
    cmap, norm = create_state_colormap()
    
    # Plot grid
    im = ax.imshow(state_matrix, cmap=cmap, norm=norm, 
                   interpolation='nearest', aspect='equal')
    
    # Highlight clusters if requested
    if highlight_clusters:
        # Find burning clusters
        burning_mask = (state_matrix == 2).astype(int)
        labeled_clusters, n_clusters = label(burning_mask)
        
        # Draw cluster boundaries
        for cluster_id in range(1, n_clusters + 1):
            cluster_mask = labeled_clusters == cluster_id
            # Find boundary pixels
            # Convert to proper boolean array
            dist_transform = distance_transform_edt(cluster_mask) > 0
            boundary = cluster_mask & ~dist_transform
            boundary_coords = np.argwhere(boundary)
            
            if len(boundary_coords) > 0:
                # Draw boundary
                for y, x in boundary_coords:
                    rect = mpatches.Rectangle((x-0.5, y-0.5), 1, 1, 
                                            fill=False, edgecolor='yellow',
                                            linewidth=2)
                    ax.add_patch(rect)
    
    # Add grid lines for small grids
    if width <= 50 and height <= 50:
        ax.set_xticks(np.arange(-0.5, width, 1), minor=True)
        ax.set_yticks(np.arange(-0.5, height, 1), minor=True)
        ax.grid(which="minor", color="gray", linestyle='-', linewidth=0.5, alpha=0.3)
    
    # Format axes
    ax.set_xlim(-0.5, width - 0.5)
    ax.set_ylim(height - 0.5, -0.5)  # Flip y-axis
    format_axis_labels(ax, xlabel='X', ylabel='Y', title=title, grid=False)
    
    # Add colorbar if requested
    if show_colorbar:
        # Create legend instead of colorbar for discrete states
        legend_elements = [mpatches.Patch(facecolor=CELL_STATE_COLORS[state], 
                                        label=state)
                          for state in ['Empty', 'Tree', 'Burning', 'Burnt']]
        ax.legend(handles=legend_elements, loc='center left', 
                 bbox_to_anchor=(1, 0.5), title='Cell State')
    
    # Add metrics if requested
    if show_metrics:
        # Calculate metrics
        n_cells = width * height
        n_trees = np.sum(state_matrix == 1)
        n_burning = np.sum(state_matrix == 2)
        n_burnt = np.sum(state_matrix == 3)
        
        tree_density = n_trees / n_cells
        burnt_fraction = n_burnt / n_cells
        
        # Add text box
        metrics_text = (f'Trees: {n_trees} ({tree_density:.1%})\n'
                       f'Burning: {n_burning}\n'
                       f'Burnt: {n_burnt} ({burnt_fraction:.1%})')
        
        ax.text(0.02, 0.98, metrics_text, transform=ax.transAxes,
               fontsize=9, verticalalignment='top',
               bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_cluster_analysis(grid_data: pd.DataFrame,
                         output_name: Optional[str] = None) -> plt.Figure:
    """
    Analyze and visualize fire cluster properties.
    
    Args:
        grid_data: DataFrame with grid data
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    # Extract state matrix
    state_matrix = extract_state_matrix(grid_data)
    
    # Find burning and burnt clusters
    fire_mask = ((state_matrix == 2) | (state_matrix == 3)).astype(int)
    labeled_clusters, n_clusters = label(fire_mask)
    
    # Calculate cluster sizes
    cluster_sizes = []
    cluster_centers = []
    
    for cluster_id in range(1, n_clusters + 1):
        cluster_mask = labeled_clusters == cluster_id
        size = np.sum(cluster_mask)
        cluster_sizes.append(size)
        
        # Find center of mass
        coords = np.argwhere(cluster_mask)
        if len(coords) > 0:
            center = coords.mean(axis=0)
            cluster_centers.append(center)
    
    cluster_sizes = np.array(cluster_sizes)
    
    # Create figure with subplots
    fig = plt.figure(figsize=(12, 8))
    
    # Grid layout
    gs = fig.add_gridspec(2, 3, hspace=0.3, wspace=0.3)
    ax_grid = fig.add_subplot(gs[:, 0])
    ax_dist = fig.add_subplot(gs[0, 1])
    ax_loglog = fig.add_subplot(gs[0, 2])
    ax_spatial = fig.add_subplot(gs[1, 1])
    ax_fractal = fig.add_subplot(gs[1, 2])
    
    # 1. Grid with clusters colored
    cluster_colors = plt.cm.tab20(np.linspace(0, 1, min(20, n_clusters)))
    colored_clusters = np.zeros((*state_matrix.shape, 3))
    
    for cluster_id in range(1, min(21, n_clusters + 1)):
        mask = labeled_clusters == cluster_id
        if cluster_id <= 20:
            colored_clusters[mask] = cluster_colors[cluster_id - 1][:3]
        else:
            colored_clusters[mask] = [0.5, 0.5, 0.5]  # Gray for remaining clusters
    
    # Add non-fire cells
    empty_mask = state_matrix == 0
    tree_mask = state_matrix == 1
    colored_clusters[empty_mask] = [0.9, 0.9, 0.9]
    colored_clusters[tree_mask] = [0.13, 0.55, 0.13]
    
    ax_grid.imshow(colored_clusters, aspect='equal')
    ax_grid.set_title('Fire Clusters')
    ax_grid.set_xlabel('X')
    ax_grid.set_ylabel('Y')
    
    # 2. Cluster size distribution
    if len(cluster_sizes) > 0:
        ax_dist.hist(cluster_sizes, bins=30, color='orange', alpha=0.7, edgecolor='black')
        ax_dist.set_xlabel('Cluster Size')
        ax_dist.set_ylabel('Count')
        ax_dist.set_title('Cluster Size Distribution')
        
        # Add statistics
        stats_text = (f'Clusters: {n_clusters}\n'
                     f'Mean size: {cluster_sizes.mean():.1f}\n'
                     f'Max size: {cluster_sizes.max()}')
        ax_dist.text(0.65, 0.95, stats_text, transform=ax_dist.transAxes,
                    fontsize=9, verticalalignment='top',
                    bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
    
    # 3. Log-log plot for power law check
    if len(cluster_sizes) > 5:
        # Create histogram for log-log plot
        unique_sizes, counts = np.unique(cluster_sizes, return_counts=True)
        
        ax_loglog.scatter(unique_sizes, counts, color='red', s=50, alpha=0.7)
        
        # Fit power law if enough data
        if len(unique_sizes) > 10:
            # Simple power law fit on log scale
            log_sizes = np.log(unique_sizes[counts > 0])
            log_counts = np.log(counts[counts > 0])
            
            if len(log_sizes) > 2:
                coeffs = np.polyfit(log_sizes, log_counts, 1)
                power_law_exponent = coeffs[0]
                
                # Plot fit
                x_fit = np.logspace(0, np.log10(unique_sizes.max()), 100)
                y_fit = np.exp(coeffs[1]) * x_fit ** coeffs[0]
                ax_loglog.plot(x_fit, y_fit, 'b--', linewidth=2,
                             label=f'τ = {-power_law_exponent:.2f}')
                ax_loglog.legend()
        
        set_log_scale(ax_loglog, x_log=True, y_log=True)
        ax_loglog.set_xlabel('Cluster Size')
        ax_loglog.set_ylabel('Frequency')
        ax_loglog.set_title('Power Law Analysis')
    
    # 4. Spatial correlation
    if len(cluster_centers) > 1:
        # Calculate pairwise distances
        centers_array = np.array(cluster_centers)
        distances = cdist(centers_array, centers_array)
        
        # Remove diagonal (self-distances)
        distances = distances[np.triu_indices_from(distances, k=1)]
        
        ax_spatial.hist(distances, bins=30, color='blue', alpha=0.7, edgecolor='black')
        ax_spatial.set_xlabel('Distance Between Clusters')
        ax_spatial.set_ylabel('Count')
        ax_spatial.set_title('Cluster Spatial Distribution')
        ax_spatial.axvline(distances.mean(), color='red', linestyle='--',
                          label=f'Mean: {distances.mean():.1f}')
        ax_spatial.legend()
    
    # 5. Fractal dimension analysis
    if len(cluster_sizes) > 0:
        # Box counting for largest cluster
        largest_cluster_id = np.argmax(cluster_sizes) + 1
        largest_cluster_mask = labeled_clusters == largest_cluster_id
        
        # Perform box counting
        box_sizes = [2, 4, 8, 16, 32]
        box_counts = []
        
        for box_size in box_sizes:
            count = 0
            for i in range(0, state_matrix.shape[0], box_size):
                for j in range(0, state_matrix.shape[1], box_size):
                    box = largest_cluster_mask[i:i+box_size, j:j+box_size]
                    if box.any():
                        count += 1
            box_counts.append(count)
        
        if len(box_counts) > 2 and all(c > 0 for c in box_counts):
            # Calculate fractal dimension
            log_sizes = np.log(box_sizes)
            log_counts = np.log(box_counts)
            
            if not np.any(np.isnan(log_counts)):
                coeffs = np.polyfit(log_sizes, log_counts, 1)
                fractal_dim = -coeffs[0]
                
                ax_fractal.scatter(box_sizes, box_counts, color='green', s=50)
                ax_fractal.plot(box_sizes, np.exp(coeffs[1]) * np.array(box_sizes) ** coeffs[0],
                               'r--', label=f'D = {fractal_dim:.2f}')
                set_log_scale(ax_fractal, x_log=True, y_log=True)
                ax_fractal.set_xlabel('Box Size')
                ax_fractal.set_ylabel('Box Count')
                ax_fractal.set_title('Fractal Dimension (Largest Cluster)')
                ax_fractal.legend()
    
    plt.suptitle('Fire Cluster Analysis', fontsize=14)
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_spatial_correlation(grid_data: pd.DataFrame,
                           max_distance: Optional[int] = None,
                           output_name: Optional[str] = None) -> plt.Figure:
    """
    Calculate and plot spatial correlation functions.
    
    Args:
        grid_data: DataFrame with grid data
        max_distance: Maximum distance for correlation calculation
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    # Extract state matrix
    state_matrix = extract_state_matrix(grid_data)
    height, width = state_matrix.shape
    
    if max_distance is None:
        max_distance = min(width, height) // 4
    
    # Create figure
    fig, (ax1, ax2) = create_figure_with_subplots(1, 2, fig_type='wide')
    
    # Calculate 2-point correlation for fire cells
    fire_mask = ((state_matrix == 2) | (state_matrix == 3)).astype(float)
    
    # Radial correlation function
    distances = np.arange(1, max_distance + 1)
    correlations = []
    
    for d in distances:
        corr_sum = 0
        count = 0
        
        # Sample pairs at distance d
        for _ in range(1000):  # Monte Carlo sampling
            # Random first point
            y1 = np.random.randint(0, height)
            x1 = np.random.randint(0, width)
            
            # Random direction
            angle = np.random.uniform(0, 2 * np.pi)
            dx = int(d * np.cos(angle))
            dy = int(d * np.sin(angle))
            
            x2 = x1 + dx
            y2 = y1 + dy
            
            # Check bounds
            if 0 <= x2 < width and 0 <= y2 < height:
                corr_sum += fire_mask[y1, x1] * fire_mask[y2, x2]
                count += 1
        
        if count > 0:
            # Normalize by average density
            avg_density = np.mean(fire_mask)
            correlation = (corr_sum / count) / (avg_density ** 2) - 1
            correlations.append(correlation)
        else:
            correlations.append(0)
    
    # Plot radial correlation
    ax1.plot(distances, correlations, 'b-', linewidth=2, marker='o')
    ax1.axhline(0, color='gray', linestyle='--', alpha=0.5)
    ax1.set_xlabel('Distance')
    ax1.set_ylabel('g(r) - 1')
    ax1.set_title('Radial Correlation Function')
    ax1.grid(True, alpha=0.3)
    
    # Find correlation length (where g(r) crosses zero or drops to 1/e)
    correlation_length = None
    for i, corr in enumerate(correlations):
        if corr <= 0 or corr <= correlations[0] / np.e:
            correlation_length = distances[i]
            ax1.axvline(correlation_length, color='red', linestyle='--',
                       label=f'ξ = {correlation_length}')
            ax1.legend()
            break
    
    # Directional correlations
    directions = ['Horizontal', 'Vertical', 'Diagonal']
    direction_correlations = {}
    
    for direction in directions:
        dir_corr = []
        
        for d in range(1, max_distance + 1):
            if direction == 'Horizontal':
                # Shift horizontally
                if d < width:
                    shifted = np.roll(fire_mask, d, axis=1)
                    valid_mask = np.ones_like(fire_mask)
                    valid_mask[:, :d] = 0
                else:
                    continue
            elif direction == 'Vertical':
                # Shift vertically
                if d < height:
                    shifted = np.roll(fire_mask, d, axis=0)
                    valid_mask = np.ones_like(fire_mask)
                    valid_mask[:d, :] = 0
                else:
                    continue
            else:  # Diagonal
                if d < min(width, height):
                    shifted = np.roll(np.roll(fire_mask, d, axis=0), d, axis=1)
                    valid_mask = np.ones_like(fire_mask)
                    valid_mask[:d, :] = 0
                    valid_mask[:, :d] = 0
                else:
                    continue
            
            # Calculate correlation
            valid_count = np.sum(valid_mask)
            if valid_count > 0:
                correlation = np.sum(fire_mask * shifted * valid_mask) / valid_count
                avg_density = np.sum(fire_mask * valid_mask) / valid_count
                if avg_density > 0:
                    normalized_corr = correlation / (avg_density ** 2) - 1
                    dir_corr.append(normalized_corr)
                else:
                    dir_corr.append(0)
        
        direction_correlations[direction] = dir_corr
    
    # Plot directional correlations
    for direction, corr_values in direction_correlations.items():
        if len(corr_values) > 0:
            ax2.plot(range(1, len(corr_values) + 1), corr_values, 
                    linewidth=2, marker='o', label=direction)
    
    ax2.axhline(0, color='gray', linestyle='--', alpha=0.5)
    ax2.set_xlabel('Distance')
    ax2.set_ylabel('g(r) - 1')
    ax2.set_title('Directional Correlations')
    ax2.legend()
    ax2.grid(True, alpha=0.3)
    
    plt.suptitle('Spatial Correlation Analysis', fontsize=12)
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_vegetation_distribution(grid_data: pd.DataFrame,
                                output_name: Optional[str] = None) -> plt.Figure:
    """
    Visualize vegetation type distribution and fire impact.
    
    Args:
        grid_data: DataFrame with grid data
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    if 'vegetation' not in grid_data.columns:
        warnings.warn("No vegetation data available")
        return None
    
    # Create figure
    fig, ((ax1, ax2), (ax3, ax4)) = create_figure_with_subplots(2, 2, fig_type='square')
    
    # Get vegetation types
    veg_types = grid_data['vegetation'].unique()
    veg_colors = [VEGETATION_COLORS.get(veg, '#808080') for veg in veg_types]
    
    # 1. Vegetation map
    width, height = infer_grid_dimensions(grid_data)
    veg_matrix = np.zeros((height, width, 3))
    
    for idx, row in grid_data.iterrows():
        if isinstance(idx, tuple):
            x, y = idx
        else:
            x, y = int(row['x']), int(row['y'])
        
        veg_type = row['vegetation']
        color = VEGETATION_COLORS.get(veg_type, '#808080')
        veg_matrix[y, x] = mcolors.to_rgb(color)
    
    ax1.imshow(veg_matrix, aspect='equal')
    ax1.set_title('Vegetation Types')
    ax1.set_xlabel('X')
    ax1.set_ylabel('Y')
    
    # 2. Vegetation distribution pie chart
    veg_counts = grid_data['vegetation'].value_counts()
    ax2.pie(veg_counts.values, labels=veg_counts.index, colors=veg_colors,
           autopct='%1.1f%%', startangle=90)
    ax2.set_title('Vegetation Distribution')
    
    # 3. Fire impact by vegetation type
    if 'state' in grid_data.columns:
        # Calculate burn rate by vegetation
        burn_rates = {}
        for veg_type in veg_types:
            veg_mask = grid_data['vegetation'] == veg_type
            veg_data = grid_data[veg_mask]
            
            total = len(veg_data)
            if total > 0:
                burnt = len(veg_data[veg_data['state'].isin(['Burning', 'Burnt'])])
                burn_rates[veg_type] = burnt / total * 100
        
        # Bar plot
        veg_names = list(burn_rates.keys())
        rates = list(burn_rates.values())
        colors = [VEGETATION_COLORS.get(veg, '#808080') for veg in veg_names]
        
        bars = ax3.bar(range(len(veg_names)), rates, color=colors, alpha=0.7)
        ax3.set_xticks(range(len(veg_names)))
        ax3.set_xticklabels(veg_names, rotation=45, ha='right')
        ax3.set_ylabel('Burn Rate (%)')
        ax3.set_title('Fire Impact by Vegetation Type')
        
        # Add value labels
        for bar, rate in zip(bars, rates):
            ax3.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 1,
                    f'{rate:.1f}%', ha='center', va='bottom', fontsize=8)
    
    # 4. Elevation vs vegetation (if elevation data available)
    if 'elevation' in grid_data.columns:
        # Box plot of elevation by vegetation type
        elevation_by_veg = []
        labels = []
        
        for veg_type in veg_types:
            veg_mask = grid_data['vegetation'] == veg_type
            elevations = grid_data.loc[veg_mask, 'elevation'].values
            if len(elevations) > 0:
                elevation_by_veg.append(elevations)
                labels.append(veg_type)
        
        bp = ax4.boxplot(elevation_by_veg, labels=labels, patch_artist=True)
        
        # Color boxes
        for patch, veg_type in zip(bp['boxes'], labels):
            patch.set_facecolor(VEGETATION_COLORS.get(veg_type, '#808080'))
            patch.set_alpha(0.7)
        
        ax4.set_xticklabels(labels, rotation=45, ha='right')
        ax4.set_ylabel('Elevation (m)')
        ax4.set_title('Elevation Distribution by Vegetation')
    
    plt.suptitle('Vegetation Analysis', fontsize=12)
    plt.tight_layout()
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def create_animation_frames(snapshots: Dict[str, pd.DataFrame],
                          output_dir: str,
                          fps: int = 10,
                          show_time: bool = True,
                          show_metrics: bool = True) -> str:
    """
    Create animation frames from grid snapshots.
    
    Args:
        snapshots: Dict of timestamp -> grid DataFrame
        output_dir: Directory to save frames
        fps: Frames per second for animation
        show_time: Whether to show timestamp
        show_metrics: Whether to show metrics overlay
        
    Returns:
        Path to saved animation
    """
    from pathlib import Path
    import os
    
    # Create output directory
    frame_dir = Path(output_dir) / 'animation_frames'
    frame_dir.mkdir(parents=True, exist_ok=True)
    
    # Sort snapshots by time
    sorted_times = sorted(snapshots.keys())
    
    # Determine grid size from first snapshot
    first_grid = snapshots[sorted_times[0]]
    width, height = infer_grid_dimensions(first_grid)
    
    # Create figure for animation
    fig, ax = plt.subplots(figsize=(8, 6))
    
    # Create colormap
    cmap, norm = create_state_colormap()
    
    # Initialize plot
    state_matrix = extract_state_matrix(first_grid)
    im = ax.imshow(state_matrix, cmap=cmap, norm=norm, 
                   interpolation='nearest', aspect='equal')
    
    ax.set_xlim(-0.5, width - 0.5)
    ax.set_ylim(height - 0.5, -0.5)
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    
    # Add elements for updating
    time_text = ax.text(0.02, 0.98, '', transform=ax.transAxes,
                       fontsize=10, verticalalignment='top',
                       bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
    
    metrics_text = ax.text(0.98, 0.98, '', transform=ax.transAxes,
                          fontsize=9, verticalalignment='top',
                          horizontalalignment='right',
                          bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
    
    # Create legend
    legend_elements = [mpatches.Patch(facecolor=CELL_STATE_COLORS[state], 
                                    label=state)
                      for state in ['Empty', 'Tree', 'Burning', 'Burnt']]
    ax.legend(handles=legend_elements, loc='center left', 
             bbox_to_anchor=(1, 0.5), title='Cell State')
    
    # Save frames
    for i, time_key in enumerate(sorted_times):
        grid_data = snapshots[time_key]
        state_matrix = extract_state_matrix(grid_data)
        
        # Update image
        im.set_array(state_matrix)
        
        # Update time text
        if show_time:
            time_text.set_text(f'Time: {time_key}')
        
        # Update metrics
        if show_metrics:
            n_cells = width * height
            n_burning = np.sum(state_matrix == 2)
            n_burnt = np.sum(state_matrix == 3)
            burnt_fraction = n_burnt / n_cells
            
            metrics_str = (f'Burning: {n_burning}\n'
                          f'Burnt: {n_burnt} ({burnt_fraction:.1%})')
            metrics_text.set_text(metrics_str)
        
        # Save frame
        frame_path = frame_dir / f'frame_{i:04d}.png'
        plt.savefig(frame_path, dpi=150, bbox_inches='tight')
        
        print(f'Saved frame {i+1}/{len(sorted_times)}', end='\r')
    
    print('\nCreating animation...')
    
    # Create GIF using imageio
    import imageio
    
    images = []
    for i in range(len(sorted_times)):
        frame_path = frame_dir / f'frame_{i:04d}.png'
        images.append(imageio.imread(frame_path))
    
    # Save as GIF
    animation_path = Path(output_dir) / 'fire_spread_animation.gif'
    imageio.mimsave(animation_path, images, fps=fps, loop=0)
    
    print(f'Animation saved to: {animation_path}')
    
    # Clean up individual frames if desired
    # for frame_file in frame_dir.glob('frame_*.png'):
    #     frame_file.unlink()
    
    plt.close(fig)
    
    return str(animation_path)


def create_spatial_summary(grid_snapshots: Dict[str, pd.DataFrame],
                          output_dir: str = "visualization/figures"):
    """
    Create comprehensive spatial analysis from grid snapshots.
    
    Args:
        grid_snapshots: Dict mapping names to grid DataFrames
        output_dir: Output directory for figures
    """
    for name, grid_data in grid_snapshots.items():
        print(f"Processing spatial patterns for {name}...")
        
        # Basic grid visualization
        fig = plot_grid_state(grid_data, title=f'Grid State: {name}',
                            highlight_clusters=True,
                            output_name=f'grid_state_{name}')
        plt.close(fig)
        
        # Cluster analysis
        fig = plot_cluster_analysis(grid_data,
                                  output_name=f'cluster_analysis_{name}')
        plt.close(fig)
        
        # Spatial correlation
        fig = plot_spatial_correlation(grid_data,
                                     output_name=f'spatial_correlation_{name}')
        plt.close(fig)
        
        # Vegetation analysis
        if 'vegetation' in grid_data.columns:
            fig = plot_vegetation_distribution(grid_data,
                                             output_name=f'vegetation_analysis_{name}')
            if fig:
                plt.close(fig)