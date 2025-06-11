"""
Phase diagram visualization for forest fire simulations.
Creates 2D heatmaps showing phase transitions with control parameters.
"""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import cm
import seaborn as sns
from scipy.interpolate import griddata
from scipy.ndimage import gaussian_filter
from typing import Optional, Tuple, List, Dict

from utils.data_loader import load_phase_data, create_phase_grid
from utils.plot_config import (create_figure_with_subplots, save_figure, 
                              format_axis_labels, add_colorbar, add_phase_boundaries,
                              add_annotation, get_figure_size)
from utils.color_schemes import (create_phase_colormap, PHASE_COLORS, 
                                get_colorbar_label, create_diverging_colormap)


def plot_phase_diagram(data: pd.DataFrame,
                      x_param: str,
                      y_param: str,
                      order_param: str = 'burnt_fraction',
                      critical_values: Optional[Dict[str, float]] = None,
                      interpolate: bool = True,
                      smooth_sigma: float = 0.5,
                      output_name: Optional[str] = None,
                      show_contours: bool = True,
                      contour_levels: Optional[List[float]] = None) -> plt.Figure:
    """
    Create a 2D phase diagram from parameter sweep data.
    
    Args:
        data: DataFrame with phase transition data
        x_param: Column name for x-axis parameter
        y_param: Column name for y-axis parameter
        order_param: Column name for order parameter to visualize
        critical_values: Dict with critical parameter values to mark
        interpolate: Whether to interpolate sparse data
        smooth_sigma: Gaussian smoothing parameter
        output_name: Filename for saving (if provided)
        show_contours: Whether to show contour lines
        contour_levels: Specific contour levels to show
        
    Returns:
        Matplotlib figure object
    """
    # Create figure
    fig, ax = create_figure_with_subplots(fig_type='phase_diagram')
    
    # Get unique parameter values
    x_unique = sorted(data[x_param].unique())
    y_unique = sorted(data[y_param].unique())
    
    if interpolate and len(x_unique) * len(y_unique) != len(data):
        # Need to interpolate sparse data
        points = data[[x_param, y_param]].values
        values = data[order_param].values
        
        # Create fine grid
        x_grid = np.linspace(min(x_unique), max(x_unique), 100)
        y_grid = np.linspace(min(y_unique), max(y_unique), 100)
        X, Y = np.meshgrid(x_grid, y_grid)
        
        # Interpolate
        Z = griddata(points, values, (X, Y), method='cubic')
        
        # Apply smoothing
        if smooth_sigma > 0:
            Z = gaussian_filter(Z, sigma=smooth_sigma)
    else:
        # Use regular grid
        X, Y, Z = create_phase_grid(data, x_param, y_param, order_param)
    
    # Create colormap
    if order_param == 'phase':
        # Discrete phases
        cmap = plt.cm.get_cmap('RdYlBu_r', 3)
        vmin, vmax = 0, 2
    else:
        # Continuous order parameter
        cmap = create_phase_colormap()
        vmin, vmax = 0, 1
    
    # Plot heatmap
    im = ax.pcolormesh(X, Y, Z, cmap=cmap, vmin=vmin, vmax=vmax, shading='auto')
    
    # Add contours
    if show_contours:
        if contour_levels is None:
            contour_levels = [0.1, 0.3, 0.5, 0.7, 0.9] if order_param != 'phase' else [0.5, 1.5]
        
        contours = ax.contour(X, Y, Z, levels=contour_levels, colors='black', 
                             linewidths=1.0, alpha=0.4)
        ax.clabel(contours, inline=True, fontsize=8, fmt='%.2f')
    
    # Add critical point markers
    if critical_values:
        if x_param in critical_values:
            ax.axvline(critical_values[x_param], color='red', linestyle='--', 
                      alpha=0.7, linewidth=2, label='Critical line')
        if y_param in critical_values:
            ax.axhline(critical_values[y_param], color='red', linestyle='--', 
                      alpha=0.7, linewidth=2)
        
        # Mark critical point if both parameters have critical values
        if x_param in critical_values and y_param in critical_values:
            ax.plot(critical_values[x_param], critical_values[y_param], 
                   'r*', markersize=15, label='Critical point')
    
    # Add phase labels if discrete phases
    if order_param == 'phase' and 'phase' in data.columns:
        # Find representative points for each phase
        for phase_name, phase_id in [('Sub-critical', 0), ('Critical', 1), ('Super-critical', 2)]:
            phase_data = data[data['phase'] == phase_name]
            if not phase_data.empty:
                # Find centroid of phase region
                x_center = phase_data[x_param].mean()
                y_center = phase_data[y_param].mean()
                add_annotation(ax, phase_name, (x_center, y_center), 
                             fontsize=10, color='white',
                             bbox_props=dict(boxstyle="round,pad=0.3", 
                                           facecolor='black', alpha=0.7))
    
    # Format axes
    format_axis_labels(ax, 
                      xlabel=x_param.replace('_', ' ').title(),
                      ylabel=y_param.replace('_', ' ').title(),
                      title=f'Phase Diagram: {order_param.replace("_", " ").title()}')
    
    # Add colorbar
    cbar_label = get_colorbar_label(order_param)
    add_colorbar(im, ax, label=cbar_label)
    
    # Add legend if critical values shown
    if critical_values:
        ax.legend(loc='best', frameon=True, fancybox=True, framealpha=0.9)
    
    # Save if requested
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_phase_boundary(data: pd.DataFrame,
                       param_name: str,
                       order_params: List[str] = ['burnt_fraction', 'percolation_indicator'],
                       output_name: Optional[str] = None) -> plt.Figure:
    """
    Plot phase boundary as function of single parameter.
    
    Args:
        data: DataFrame with phase transition data
        param_name: Parameter name for x-axis
        order_params: List of order parameters to plot
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    fig, ax = create_figure_with_subplots()
    
    colors = plt.cm.get_cmap('tab10')
    
    for i, order_param in enumerate(order_params):
        if order_param in data.columns:
            # Group by parameter value and calculate mean/std
            grouped = data.groupby(param_name)[order_param].agg(['mean', 'std'])
            
            # Plot with error bars
            ax.errorbar(grouped.index, grouped['mean'], yerr=grouped['std'],
                       label=order_param.replace('_', ' ').title(),
                       color=colors(i), marker='o', capsize=5, capthick=1)
    
    # Mark phase transition region
    if 'phase' in data.columns:
        # Find transition points
        critical_data = data[data['phase'] == 'Critical']
        if not critical_data.empty:
            param_range = [critical_data[param_name].min(), critical_data[param_name].max()]
            ax.axvspan(param_range[0], param_range[1], alpha=0.2, color='gold',
                      label='Critical region')
    
    format_axis_labels(ax,
                      xlabel=param_name.replace('_', ' ').title(),
                      ylabel='Order Parameter',
                      title='Phase Transition')
    
    ax.legend(loc='best')
    ax.set_ylim([0, 1.05])
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_multi_phase_diagrams(datasets: Dict[str, pd.DataFrame],
                             x_param: str,
                             y_param: str,
                             order_param: str = 'burnt_fraction',
                             output_name: Optional[str] = None) -> plt.Figure:
    """
    Create multiple phase diagrams for comparison (e.g., different scenarios).
    
    Args:
        datasets: Dict mapping labels to DataFrames
        x_param: Column name for x-axis parameter
        y_param: Column name for y-axis parameter
        order_param: Column name for order parameter
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    n_plots = len(datasets)
    n_cols = min(3, n_plots)
    n_rows = (n_plots + n_cols - 1) // n_cols
    
    fig, axes = create_figure_with_subplots(n_rows, n_cols, fig_type='wide',
                                           sharex=True, sharey=True)
    axes = axes.flatten() if n_plots > 1 else [axes]
    
    # Use same colormap and scale for all plots
    cmap = create_phase_colormap()
    vmin = min(df[order_param].min() for df in datasets.values())
    vmax = max(df[order_param].max() for df in datasets.values())
    
    for idx, (label, data) in enumerate(datasets.items()):
        ax = axes[idx]
        
        # Create grid
        X, Y, Z = create_phase_grid(data, x_param, y_param, order_param)
        
        # Plot
        im = ax.pcolormesh(X, Y, Z, cmap=cmap, vmin=vmin, vmax=vmax, shading='auto')
        
        # Add contours
        contours = ax.contour(X, Y, Z, levels=[0.3, 0.5, 0.7], colors='black',
                             linewidths=0.8, alpha=0.4)
        
        # Format
        format_axis_labels(ax, 
                          xlabel=x_param.replace('_', ' ').title() if idx >= n_plots - n_cols else None,
                          ylabel=y_param.replace('_', ' ').title() if idx % n_cols == 0 else None,
                          title=label)
    
    # Remove empty subplots
    for idx in range(n_plots, len(axes)):
        fig.delaxes(axes[idx])
    
    # Add single colorbar
    cbar_ax = fig.add_axes([0.92, 0.15, 0.02, 0.7])
    cbar = fig.colorbar(im, cax=cbar_ax)
    cbar.set_label(get_colorbar_label(order_param), rotation=270, labelpad=15)
    
    plt.suptitle(f'Phase Diagrams: {order_param.replace("_", " ").title()}', y=0.98)
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_susceptibility_peak(data: pd.DataFrame,
                            param_name: str,
                            system_sizes: Optional[List[int]] = None,
                            output_name: Optional[str] = None) -> plt.Figure:
    """
    Plot susceptibility vs parameter to identify critical point.
    
    Args:
        data: DataFrame with phase transition data
        param_name: Parameter name for x-axis
        system_sizes: List of system sizes if finite-size scaling data available
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    fig, (ax1, ax2) = create_figure_with_subplots(2, 1, fig_type='tall', sharex=True)
    
    # Calculate susceptibility (variance of order parameter)
    if system_sizes and 'system_size' in data.columns:
        # Finite-size scaling analysis
        colors = plt.cm.get_cmap('viridis', len(system_sizes))
        
        for i, L in enumerate(system_sizes):
            size_data = data[data['system_size'] == L]
            grouped = size_data.groupby(param_name)['burnt_fraction'].agg(['mean', 'var'])
            
            # Plot order parameter
            ax1.plot(grouped.index, grouped['mean'], 'o-', 
                    label=f'L={L}', color=colors(i))
            
            # Plot susceptibility (variance)
            ax2.plot(grouped.index, grouped['var'], 'o-', 
                    label=f'L={L}', color=colors(i))
    else:
        # Single system size
        grouped = data.groupby(param_name)['burnt_fraction'].agg(['mean', 'var'])
        
        ax1.plot(grouped.index, grouped['mean'], 'o-', color='blue')
        ax2.plot(grouped.index, grouped['var'], 'o-', color='red')
    
    # Find and mark susceptibility peak
    max_idx = grouped['var'].idxmax()
    ax2.axvline(max_idx, color='red', linestyle='--', alpha=0.7)
    add_annotation(ax2, f'Peak at {max_idx:.3f}', 
                  (max_idx, grouped.loc[max_idx, 'var']),
                  xytext=(max_idx * 1.1, grouped.loc[max_idx, 'var'] * 0.9))
    
    format_axis_labels(ax1, ylabel='Order Parameter', 
                      title='Critical Point from Susceptibility Peak')
    format_axis_labels(ax2, xlabel=param_name.replace('_', ' ').title(),
                      ylabel='Susceptibility (χ)')
    
    if system_sizes:
        ax1.legend(loc='best')
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def create_phase_diagram_summary(phase_data_files: Dict[str, str],
                                output_dir: str = "visualization/figures"):
    """
    Create comprehensive phase diagram analysis from multiple data files.
    
    Args:
        phase_data_files: Dict mapping diagram names to file paths
        output_dir: Output directory for figures
    """
    for name, filepath in phase_data_files.items():
        print(f"Processing {name}...")
        
        # Load data
        data = load_phase_data(filepath)
        
        # Determine parameters
        numeric_cols = data.select_dtypes(include=[np.number]).columns
        param_cols = [col for col in numeric_cols 
                     if col not in ['burnt_fraction', 'percolation_indicator', 
                                   'largest_cluster', 'active_fires']]
        
        if len(param_cols) >= 2:
            # 2D phase diagram
            fig = plot_phase_diagram(data, param_cols[0], param_cols[1],
                                   order_param='burnt_fraction',
                                   output_name=f'phase_diagram_{name}')
            plt.close(fig)
        
        if len(param_cols) >= 1:
            # 1D phase boundary
            fig = plot_phase_boundary(data, param_cols[0],
                                    output_name=f'phase_boundary_{name}')
            plt.close(fig)
            
            # Susceptibility analysis
            fig = plot_susceptibility_peak(data, param_cols[0],
                                         output_name=f'susceptibility_{name}')
            plt.close(fig)