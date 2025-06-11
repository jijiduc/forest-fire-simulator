"""
Time series analysis and visualization for forest fire simulations.
Creates multi-panel plots showing temporal evolution of fire metrics.
"""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
from scipy.signal import savgol_filter
from scipy.interpolate import interp1d
from scipy.stats import linregress
from typing import Optional, List, Dict, Tuple
import warnings

from utils.data_loader import load_timeseries, load_simulation_output
from utils.plot_config import (create_figure_with_subplots, save_figure,
                              format_axis_labels, get_figure_size, set_log_scale,
                              add_annotation)
from utils.color_schemes import (SCENARIO_COLORS, get_scenario_color,
                                create_qualitative_palette, get_colorbar_label)


def plot_time_series(data: pd.DataFrame,
                    metrics: List[str] = ['active_fires', 'burnt_area', 'percolation_indicator'],
                    smooth: bool = True,
                    smooth_window: int = 5,
                    show_trends: bool = True,
                    mark_transitions: bool = True,
                    output_name: Optional[str] = None) -> plt.Figure:
    """
    Create multi-panel time series plot with key metrics.
    
    Args:
        data: DataFrame with time series data (time as index)
        metrics: List of column names to plot
        smooth: Whether to apply smoothing
        smooth_window: Window size for smoothing
        show_trends: Whether to show trend lines
        mark_transitions: Whether to mark phase transitions
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    # Filter available metrics
    available_metrics = [m for m in metrics if m in data.columns]
    n_panels = len(available_metrics)
    
    fig, axes = create_figure_with_subplots(n_panels, 1, fig_type='time_series',
                                           sharex=True)
    if n_panels == 1:
        axes = [axes]
    
    colors = create_qualitative_palette(n_panels)
    
    for idx, (ax, metric) in enumerate(zip(axes, available_metrics)):
        # Get data
        y_data = data[metric].values
        x_data = data.index.values
        
        # Plot raw data
        ax.plot(x_data, y_data, color=colors[idx], alpha=0.5, 
               linewidth=1, label='Raw data')
        
        # Apply smoothing if requested
        if smooth and len(y_data) > smooth_window:
            try:
                y_smooth = savgol_filter(y_data, smooth_window, 3)
                ax.plot(x_data, y_smooth, color=colors[idx], linewidth=2,
                       label='Smoothed')
            except:
                # Fallback to simple moving average
                y_smooth = pd.Series(y_data).rolling(smooth_window, center=True).mean()
                ax.plot(x_data, y_smooth, color=colors[idx], linewidth=2,
                       label='Moving avg')
        
        # Add trend line if requested
        if show_trends and len(x_data) > 10:
            # Linear regression on latter half of data
            mid_point = len(x_data) // 2
            slope, intercept, r_value, _, _ = linregress(x_data[mid_point:], 
                                                         y_data[mid_point:])
            trend_line = slope * x_data + intercept
            ax.plot(x_data, trend_line, '--', color='gray', alpha=0.7,
                   label=f'Trend (R²={r_value**2:.3f})')
        
        # Mark phase transitions
        if mark_transitions and metric == 'percolation_indicator':
            # Find transition points (crossing 0.5)
            transitions = np.where(np.diff(np.sign(y_data - 0.5)))[0]
            for t_idx in transitions:
                if t_idx < len(x_data) - 1:
                    ax.axvline(x_data[t_idx], color='red', linestyle=':', 
                             alpha=0.7, linewidth=1)
            
            # Add critical threshold line
            ax.axhline(0.5, color='red', linestyle='--', alpha=0.5,
                      label='Critical threshold')
        
        # Special handling for specific metrics
        if metric == 'tree_density':
            ax.set_ylim([0, 1])
        elif metric == 'percolation_indicator':
            ax.set_ylim([0, 1.05])
            ax.fill_between(x_data, 0, y_data, where=(y_data > 0.5),
                           color='red', alpha=0.1, label='Percolating')
        elif metric in ['active_fires', 'burnt_area', 'largest_cluster']:
            # These can benefit from log scale if values span orders of magnitude
            if y_data.max() > 0 and y_data.max() / (y_data[y_data > 0].min()) > 100:
                set_log_scale(ax, y_log=True)
        
        # Format
        ylabel = metric.replace('_', ' ').title()
        format_axis_labels(ax, ylabel=ylabel, grid=True)
        
        # Legend for first and last panels
        if idx == 0 or idx == n_panels - 1:
            ax.legend(loc='best', fontsize=8)
    
    # X-axis label only on bottom panel
    axes[-1].set_xlabel('Time (hours)')
    
    # Overall title
    plt.suptitle('Fire Dynamics Evolution', y=0.995)
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_multi_scenario_comparison(scenarios_data: Dict[str, pd.DataFrame],
                                  metric: str = 'burnt_area',
                                  normalize: bool = False,
                                  output_name: Optional[str] = None) -> plt.Figure:
    """
    Compare time series across multiple scenarios.
    
    Args:
        scenarios_data: Dict mapping scenario names to DataFrames
        metric: Metric to compare
        normalize: Whether to normalize values to [0, 1]
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    fig, ax = create_figure_with_subplots()
    
    # Get colors for scenarios
    max_value = 0
    
    for scenario_name, data in scenarios_data.items():
        if metric in data.columns:
            color = get_scenario_color(scenario_name)
            y_data = data[metric].values
            x_data = data.index.values
            
            if normalize and y_data.max() > 0:
                y_data = y_data / y_data.max()
            
            max_value = max(max_value, y_data.max())
            
            # Plot with scenario-specific styling
            if 'baseline' in scenario_name.lower():
                ax.plot(x_data, y_data, color=color, linewidth=2.5,
                       label=scenario_name, linestyle='-')
            else:
                ax.plot(x_data, y_data, color=color, linewidth=2,
                       label=scenario_name, alpha=0.8)
    
    # Format
    format_axis_labels(ax,
                      xlabel='Time (hours)',
                      ylabel=f'{metric.replace("_", " ").title()}{"(normalized)" if normalize else ""}',
                      title=f'Scenario Comparison: {metric.replace("_", " ").title()}')
    
    # Set y-limits
    if metric == 'percolation_indicator' or normalize:
        ax.set_ylim([0, 1.05])
    else:
        ax.set_ylim([0, max_value * 1.1])
    
    ax.legend(loc='best')
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_phase_evolution(data: pd.DataFrame,
                        phase_boundaries: Optional[Dict[str, float]] = None,
                        output_name: Optional[str] = None) -> plt.Figure:
    """
    Plot evolution showing different phases of fire spread.
    
    Args:
        data: DataFrame with time series data
        phase_boundaries: Dict with time boundaries for phases
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    fig, (ax1, ax2, ax3) = create_figure_with_subplots(3, 1, fig_type='time_series',
                                                       sharex=True)
    
    time = data.index.values
    
    # Panel 1: Fire activity
    if 'active_fires' in data.columns:
        ax1.plot(time, data['active_fires'], color='orange', linewidth=2)
        ax1.fill_between(time, 0, data['active_fires'], color='orange', alpha=0.3)
        format_axis_labels(ax1, ylabel='Active Fires', title='Fire Spread Phases')
    
    # Panel 2: Cumulative burnt area
    if 'burnt_area' in data.columns:
        ax2.plot(time, data['burnt_area'], color='darkred', linewidth=2)
        ax2.fill_between(time, 0, data['burnt_area'], color='darkred', alpha=0.3)
        
        # Add growth rate as secondary y-axis
        ax2_twin = ax2.twinx()
        growth_rate = np.gradient(data['burnt_area'])
        ax2_twin.plot(time, growth_rate, color='red', linestyle='--', 
                     alpha=0.7, linewidth=1)
        ax2_twin.set_ylabel('Growth Rate', color='red')
        ax2_twin.tick_params(axis='y', labelcolor='red')
        
        format_axis_labels(ax2, ylabel='Burnt Area')
    
    # Panel 3: Percolation indicator
    if 'percolation_indicator' in data.columns:
        ax3.plot(time, data['percolation_indicator'], color='blue', linewidth=2)
        ax3.axhline(0.5, color='red', linestyle='--', alpha=0.5)
        ax3.fill_between(time, 0, data['percolation_indicator'], 
                        where=(data['percolation_indicator'] > 0.5),
                        color='red', alpha=0.2, label='Percolating')
        ax3.fill_between(time, 0, data['percolation_indicator'],
                        where=(data['percolation_indicator'] <= 0.5),
                        color='blue', alpha=0.2, label='Non-percolating')
        format_axis_labels(ax3, xlabel='Time (hours)', 
                          ylabel='Percolation', grid=True)
        ax3.set_ylim([0, 1.05])
        ax3.legend(loc='right')
    
    # Add phase boundaries if provided
    if phase_boundaries:
        for phase_name, boundary_time in phase_boundaries.items():
            for ax in [ax1, ax2, ax3]:
                ax.axvline(boundary_time, color='gray', linestyle=':', alpha=0.7)
            # Add label only to top plot
            ax1.text(boundary_time, ax1.get_ylim()[1] * 0.9, phase_name,
                    rotation=90, va='top', ha='right', fontsize=8)
    
    # Identify phases automatically if not provided
    if not phase_boundaries and 'percolation_indicator' in data.columns:
        perc = data['percolation_indicator'].values
        
        # Ignition phase: first non-zero activity
        ignition_idx = np.where(perc > 0)[0]
        if len(ignition_idx) > 0:
            ignition_time = time[ignition_idx[0]]
            ax1.axvspan(0, ignition_time, alpha=0.1, color='green', label='Pre-ignition')
        
        # Growth phase: rapid increase
        if len(perc) > 10:
            growth_rate = np.gradient(perc)
            max_growth_idx = np.argmax(growth_rate)
            ax1.axvspan(ignition_time, time[max_growth_idx], alpha=0.1, 
                       color='orange', label='Growth')
        
        # Percolation phase: indicator > 0.5
        perc_idx = np.where(perc > 0.5)[0]
        if len(perc_idx) > 0:
            perc_start = time[perc_idx[0]]
            perc_end = time[perc_idx[-1]] if len(perc_idx) > 1 else time[-1]
            ax1.axvspan(perc_start, perc_end, alpha=0.1, color='red', 
                       label='Percolation')
    
    plt.tight_layout()
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_seasonal_analysis(data: pd.DataFrame,
                          season_length: int = 90,  # days
                          output_name: Optional[str] = None) -> plt.Figure:
    """
    Analyze seasonal patterns in fire dynamics.
    
    Args:
        data: DataFrame with time series data
        season_length: Length of season in time units
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    fig, (ax1, ax2) = create_figure_with_subplots(2, 1, fig_type='time_series',
                                                  sharex=True)
    
    time = data.index.values
    
    # Calculate seasonal indices
    season_idx = (time // season_length).astype(int)
    n_seasons = season_idx.max() + 1
    
    # Panel 1: Raw data with seasonal shading
    if 'burnt_area' in data.columns:
        ax1.plot(time, data['burnt_area'], color='darkred', linewidth=1.5)
        
        # Add seasonal shading
        season_colors = ['lightblue', 'lightyellow', 'lightgreen', 'lightcoral']
        for s in range(n_seasons):
            season_mask = season_idx == s
            if season_mask.any():
                season_start = time[season_mask].min()
                season_end = time[season_mask].max()
                ax1.axvspan(season_start, season_end, alpha=0.2,
                           color=season_colors[s % len(season_colors)])
        
        format_axis_labels(ax1, ylabel='Burnt Area', title='Seasonal Fire Patterns')
    
    # Panel 2: Seasonal aggregates
    if 'active_fires' in data.columns:
        # Calculate seasonal statistics
        seasonal_stats = []
        for s in range(n_seasons):
            season_mask = season_idx == s
            if season_mask.any():
                season_data = data.loc[season_mask, 'active_fires']
                seasonal_stats.append({
                    'season': s,
                    'mean': season_data.mean(),
                    'max': season_data.max(),
                    'total': season_data.sum()
                })
        
        seasonal_df = pd.DataFrame(seasonal_stats)
        
        # Bar plot of seasonal totals
        x_pos = np.arange(len(seasonal_df))
        bars = ax2.bar(x_pos, seasonal_df['total'], color='orange', alpha=0.7)
        
        # Add mean line
        ax2_twin = ax2.twinx()
        ax2_twin.plot(x_pos, seasonal_df['mean'], 'ro-', linewidth=2,
                     markersize=8, label='Mean activity')
        ax2_twin.set_ylabel('Mean Active Fires', color='red')
        ax2_twin.tick_params(axis='y', labelcolor='red')
        
        ax2.set_xticks(x_pos)
        ax2.set_xticklabels([f'S{i+1}' for i in range(len(seasonal_df))])
        format_axis_labels(ax2, xlabel='Season', ylabel='Total Fire Activity')
    
    plt.tight_layout()
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_correlation_analysis(data: pd.DataFrame,
                             metrics: List[str] = None,
                             lag_range: int = 10,
                             output_name: Optional[str] = None) -> plt.Figure:
    """
    Analyze correlations and time lags between metrics.
    
    Args:
        data: DataFrame with time series data
        metrics: List of metrics to analyze (default: all numeric columns)
        lag_range: Maximum lag to consider
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    if metrics is None:
        metrics = [col for col in data.select_dtypes(include=[np.number]).columns
                  if col != 'time']
    
    n_metrics = len(metrics)
    fig, axes = plt.subplots(n_metrics, n_metrics, 
                            figsize=(2.5 * n_metrics, 2.5 * n_metrics))
    
    for i, metric1 in enumerate(metrics):
        for j, metric2 in enumerate(metrics):
            ax = axes[i, j] if n_metrics > 1 else axes
            
            if i == j:
                # Autocorrelation on diagonal
                autocorr = [data[metric1].autocorr(lag=k) for k in range(lag_range)]
                ax.plot(range(lag_range), autocorr, 'b-')
                ax.axhline(0, color='gray', linestyle='--', alpha=0.5)
                ax.set_title(f'{metric1}\nautocorr', fontsize=9)
                ax.set_ylim([-1, 1])
            else:
                # Cross-correlation
                # Find optimal lag
                correlations = []
                for lag in range(-lag_range, lag_range + 1):
                    if lag < 0:
                        corr = data[metric1].iloc[:lag].corr(data[metric2].iloc[-lag:])
                    elif lag > 0:
                        corr = data[metric1].iloc[lag:].corr(data[metric2].iloc[:-lag])
                    else:
                        corr = data[metric1].corr(data[metric2])
                    correlations.append(corr)
                
                # Plot correlation vs lag
                lags = range(-lag_range, lag_range + 1)
                ax.plot(lags, correlations, 'g-')
                ax.axhline(0, color='gray', linestyle='--', alpha=0.5)
                ax.axvline(0, color='gray', linestyle='--', alpha=0.5)
                
                # Mark maximum correlation
                max_corr_idx = np.nanargmax(np.abs(correlations))
                max_lag = list(lags)[max_corr_idx]
                max_corr = correlations[max_corr_idx]
                ax.plot(max_lag, max_corr, 'ro', markersize=8)
                ax.text(0.05, 0.95, f'Max: {max_corr:.2f} @ lag {max_lag}',
                       transform=ax.transAxes, fontsize=8, va='top')
                
                ax.set_ylim([-1, 1])
            
            # Labels
            if i == n_metrics - 1:
                ax.set_xlabel('Lag', fontsize=8)
            if j == 0:
                ax.set_ylabel('Correlation', fontsize=8)
    
    plt.suptitle('Cross-Correlation Analysis', fontsize=12)
    plt.tight_layout()
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def create_time_series_summary(simulation_outputs: Dict[str, str],
                              output_dir: str = "visualization/figures"):
    """
    Create comprehensive time series analysis from simulation outputs.
    
    Args:
        simulation_outputs: Dict mapping names to output directories
        output_dir: Output directory for figures
    """
    all_scenarios = {}
    
    for name, output_path in simulation_outputs.items():
        print(f"Processing time series for {name}...")
        
        # Load all data from directory
        sim_data = load_simulation_output(output_path)
        
        if 'timeseries' in sim_data:
            for scenario_name, ts_data in sim_data['timeseries'].items():
                all_scenarios[scenario_name] = ts_data
                
                # Individual time series plot
                fig = plot_time_series(ts_data, 
                                     output_name=f'timeseries_{scenario_name}')
                plt.close(fig)
                
                # Phase evolution
                fig = plot_phase_evolution(ts_data,
                                         output_name=f'phase_evolution_{scenario_name}')
                plt.close(fig)
    
    # Multi-scenario comparison
    if len(all_scenarios) > 1:
        for metric in ['burnt_area', 'percolation_indicator', 'active_fires']:
            fig = plot_multi_scenario_comparison(all_scenarios, metric=metric,
                                               output_name=f'scenario_comparison_{metric}')
            plt.close(fig)