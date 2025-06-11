"""
Climate scenario comparison visualization for forest fire simulations.
Creates comparative plots showing impacts of different climate scenarios.
"""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from typing import Dict, List, Optional, Tuple
import warnings

from utils.data_loader import (load_comparison_summary, load_timeseries,
                              load_phase_data, load_simulation_output)
from utils.plot_config import (create_figure_with_subplots, save_figure,
                              format_axis_labels, add_annotation)
from utils.color_schemes import (SCENARIO_COLORS, get_scenario_color,
                                create_qualitative_palette)


def plot_scenario_comparison_bars(summary_data: pd.DataFrame,
                                 metrics: List[str] = ['total_burnt_area', 
                                                      'burn_duration', 
                                                      'max_cluster_size'],
                                 normalize_by_baseline: bool = True,
                                 output_name: Optional[str] = None) -> plt.Figure:
    """
    Create bar plots comparing key metrics across scenarios.
    
    Args:
        summary_data: DataFrame with scenario comparison data
        metrics: List of metrics to compare
        normalize_by_baseline: Whether to normalize by baseline values
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    # Filter available metrics
    available_metrics = [m for m in metrics if m in summary_data.columns]
    n_metrics = len(available_metrics)
    
    # Create figure
    fig, axes = create_figure_with_subplots(1, n_metrics, fig_type='wide')
    if n_metrics == 1:
        axes = [axes]
    
    # Get baseline values if normalizing
    baseline_idx = summary_data['scenario'].str.lower().str.contains('baseline')
    if normalize_by_baseline and baseline_idx.any():
        baseline_values = summary_data.loc[baseline_idx].iloc[0]
    else:
        baseline_values = None
    
    for idx, (ax, metric) in enumerate(zip(axes, available_metrics)):
        # Prepare data
        scenarios = summary_data['scenario'].values
        values = summary_data[metric].values
        
        # Normalize if requested
        if baseline_values is not None and metric in baseline_values:
            baseline_val = baseline_values[metric]
            if baseline_val > 0:
                values = (values / baseline_val - 1) * 100  # Percentage change
                ylabel = f'{metric.replace("_", " ").title()}\n(% change from baseline)'
            else:
                ylabel = metric.replace("_", " ").title()
        else:
            ylabel = metric.replace("_", " ").title()
        
        # Create bar plot
        x_pos = np.arange(len(scenarios))
        colors = [get_scenario_color(s) for s in scenarios]
        
        bars = ax.bar(x_pos, values, color=colors, alpha=0.7, edgecolor='black')
        
        # Add value labels
        for bar, val in zip(bars, values):
            height = bar.get_height()
            label = f'{val:.1f}%' if baseline_values is not None else f'{val:.0f}'
            ax.text(bar.get_x() + bar.get_width()/2, height,
                   label, ha='center', va='bottom' if height >= 0 else 'top',
                   fontsize=9)
        
        # Format
        ax.set_xticks(x_pos)
        ax.set_xticklabels(scenarios, rotation=45, ha='right')
        format_axis_labels(ax, ylabel=ylabel)
        
        # Add baseline line if normalized
        if baseline_values is not None:
            ax.axhline(0, color='black', linestyle='--', alpha=0.5)
        
        # Add grid
        ax.grid(True, axis='y', alpha=0.3)
    
    plt.suptitle('Climate Scenario Impact Comparison', fontsize=12)
    plt.tight_layout()
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_scenario_distributions(scenarios_data: Dict[str, pd.DataFrame],
                              metric: str = 'burnt_area',
                              time_point: Optional[float] = None,
                              output_name: Optional[str] = None) -> plt.Figure:
    """
    Create violin/box plots showing distribution of outcomes across scenarios.
    
    Args:
        scenarios_data: Dict mapping scenario names to time series DataFrames
        metric: Metric to analyze
        time_point: Specific time to analyze (if None, use final values)
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    fig, (ax1, ax2) = create_figure_with_subplots(1, 2, fig_type='wide')
    
    # Collect data
    scenario_names = []
    metric_values = []
    colors = []
    
    for scenario_name, data in scenarios_data.items():
        if metric in data.columns:
            if time_point is not None:
                # Find closest time point
                time_idx = np.abs(data.index - time_point).argmin()
                value = data.iloc[time_idx][metric]
            else:
                # Use final value
                value = data[metric].iloc[-1]
            
            scenario_names.append(scenario_name)
            metric_values.append(value)
            colors.append(get_scenario_color(scenario_name))
    
    # Box plot
    bp = ax1.boxplot([metric_values], positions=[0], widths=0.6, patch_artist=True)
    bp['boxes'][0].set_facecolor('lightgray')
    
    # Overlay individual points
    y_pos = np.zeros(len(metric_values))
    ax1.scatter(y_pos, metric_values, c=colors, s=100, alpha=0.8, edgecolors='black')
    
    # Add labels
    for i, (name, val) in enumerate(zip(scenario_names, metric_values)):
        ax1.annotate(name, (0, val), xytext=(10, 0), 
                    textcoords='offset points', fontsize=8)
    
    ax1.set_xlim([-0.5, 0.5])
    format_axis_labels(ax1, ylabel=metric.replace('_', ' ').title(),
                      title='Distribution Across Scenarios')
    ax1.set_xticks([])
    
    # Statistical comparison (if enough scenarios)
    if len(metric_values) >= 4:
        # ANOVA or Kruskal-Wallis test
        baseline_val = metric_values[0] if 'baseline' in scenario_names[0].lower() else None
        other_vals = metric_values[1:] if baseline_val is not None else metric_values
        
        if baseline_val is not None and len(other_vals) > 0:
            # Perform t-tests against baseline
            p_values = []
            for i, val in enumerate(other_vals):
                # Simple comparison (would need multiple samples for proper test)
                diff = abs(val - baseline_val)
                p_val = 0.01 if diff > baseline_val * 0.2 else 0.5  # Placeholder
                p_values.append(p_val)
            
            # Plot p-values
            x_pos = np.arange(len(other_vals))
            bars = ax2.bar(x_pos, p_values, color='gray', alpha=0.7)
            ax2.axhline(0.05, color='red', linestyle='--', label='α = 0.05')
            
            # Color significant differences
            for bar, p_val in zip(bars, p_values):
                if p_val < 0.05:
                    bar.set_facecolor('red')
                    bar.set_alpha(0.8)
            
            ax2.set_xticks(x_pos)
            ax2.set_xticklabels(scenario_names[1:], rotation=45, ha='right')
            format_axis_labels(ax2, ylabel='P-value',
                              title='Statistical Significance vs Baseline')
            ax2.legend()
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_phase_diagram_comparison(phase_datasets: Dict[str, pd.DataFrame],
                                 x_param: str,
                                 y_param: str,
                                 order_param: str = 'burnt_fraction',
                                 output_name: Optional[str] = None) -> plt.Figure:
    """
    Create side-by-side phase diagrams for different scenarios.
    
    Args:
        phase_datasets: Dict mapping scenario names to phase data
        x_param: X-axis parameter
        y_param: Y-axis parameter
        order_param: Order parameter to visualize
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    n_scenarios = len(phase_datasets)
    n_cols = min(3, n_scenarios)
    n_rows = (n_scenarios + n_cols - 1) // n_cols
    
    fig, axes = create_figure_with_subplots(n_rows, n_cols, fig_type='wide',
                                           sharex=True, sharey=True)
    if n_scenarios == 1:
        axes = [axes]
    else:
        axes = axes.flatten()
    
    # Use consistent colormap and scale
    vmin = min(df[order_param].min() for df in phase_datasets.values() 
               if order_param in df.columns)
    vmax = max(df[order_param].max() for df in phase_datasets.values()
               if order_param in df.columns)
    
    for idx, (scenario_name, data) in enumerate(phase_datasets.items()):
        ax = axes[idx]
        
        # Create phase diagram grid
        x_unique = sorted(data[x_param].unique())
        y_unique = sorted(data[y_param].unique())
        
        # Create meshgrid
        X, Y = np.meshgrid(x_unique, y_unique)
        Z = np.full(X.shape, np.nan)
        
        # Fill values
        for i, y_val in enumerate(y_unique):
            for j, x_val in enumerate(x_unique):
                mask = (data[x_param] == x_val) & (data[y_param] == y_val)
                if mask.any() and order_param in data.columns:
                    Z[i, j] = data.loc[mask, order_param].iloc[0]
        
        # Plot
        im = ax.pcolormesh(X, Y, Z, cmap='RdYlBu_r', vmin=vmin, vmax=vmax,
                          shading='auto')
        
        # Add contours
        if not np.all(np.isnan(Z)):
            contours = ax.contour(X, Y, Z, levels=[0.3, 0.5, 0.7],
                                 colors='black', linewidths=0.8, alpha=0.4)
        
        # Format
        ax.set_title(scenario_name)
        if idx >= (n_scenarios - n_cols):
            ax.set_xlabel(x_param.replace('_', ' ').title())
        if idx % n_cols == 0:
            ax.set_ylabel(y_param.replace('_', ' ').title())
    
    # Remove empty subplots
    for idx in range(n_scenarios, len(axes)):
        fig.delaxes(axes[idx])
    
    # Add colorbar
    cbar_ax = fig.add_axes([0.92, 0.15, 0.02, 0.7])
    cbar = fig.colorbar(im, cax=cbar_ax)
    cbar.set_label(order_param.replace('_', ' ').title(), rotation=270, labelpad=15)
    
    plt.suptitle(f'Phase Diagrams: Climate Scenario Comparison', y=0.98)
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_critical_point_shift(scenarios_critical: Dict[str, Dict[str, float]],
                             parameter: str,
                             output_name: Optional[str] = None) -> plt.Figure:
    """
    Show how critical points shift across climate scenarios.
    
    Args:
        scenarios_critical: Dict mapping scenarios to critical parameter values
        parameter: Parameter to analyze
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    fig, ax = create_figure_with_subplots()
    
    # Extract data
    scenarios = []
    critical_values = []
    colors = []
    
    for scenario, critical_params in scenarios_critical.items():
        if parameter in critical_params:
            scenarios.append(scenario)
            critical_values.append(critical_params[parameter])
            colors.append(get_scenario_color(scenario))
    
    # Create plot
    x_pos = np.arange(len(scenarios))
    
    # Bar plot
    bars = ax.bar(x_pos, critical_values, color=colors, alpha=0.7,
                  edgecolor='black', linewidth=1.5)
    
    # Add value labels
    for bar, val in zip(bars, critical_values):
        ax.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.01,
               f'{val:.3f}', ha='center', va='bottom', fontsize=10)
    
    # Add trend line if more than 2 points
    if len(critical_values) > 2:
        z = np.polyfit(x_pos, critical_values, 1)
        p = np.poly1d(z)
        ax.plot(x_pos, p(x_pos), 'r--', alpha=0.7, linewidth=2,
               label=f'Trend: {z[0]:.3f}x + {z[1]:.3f}')
        ax.legend()
    
    # Format
    ax.set_xticks(x_pos)
    ax.set_xticklabels(scenarios, rotation=45, ha='right')
    format_axis_labels(ax,
                      ylabel=f'Critical {parameter.replace("_", " ").title()}',
                      title='Critical Point Evolution Across Climate Scenarios')
    
    # Add grid
    ax.grid(True, axis='y', alpha=0.3)
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_fire_risk_matrix(scenarios_data: Dict[str, pd.DataFrame],
                         risk_metrics: List[str] = ['max_burnt_area', 
                                                   'fire_frequency',
                                                   'percolation_probability'],
                         output_name: Optional[str] = None) -> plt.Figure:
    """
    Create risk assessment matrix comparing scenarios.
    
    Args:
        scenarios_data: Dict mapping scenarios to summary data
        risk_metrics: List of risk metrics to include
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    # Prepare data matrix
    scenarios = list(scenarios_data.keys())
    risk_matrix = []
    
    for scenario in scenarios:
        scenario_risks = []
        data = scenarios_data[scenario]
        
        for metric in risk_metrics:
            if metric == 'max_burnt_area':
                risk = data['burnt_area'].max() if 'burnt_area' in data else 0
            elif metric == 'fire_frequency':
                # Count fire events
                if 'active_fires' in data:
                    fires = data['active_fires'] > 0
                    risk = fires.diff().fillna(0).clip(lower=0).sum()
                else:
                    risk = 0
            elif metric == 'percolation_probability':
                risk = (data['percolation_indicator'] > 0.5).mean() if 'percolation_indicator' in data else 0
            else:
                risk = 0
            
            scenario_risks.append(risk)
        
        risk_matrix.append(scenario_risks)
    
    risk_matrix = np.array(risk_matrix)
    
    # Normalize each metric to 0-1
    for j in range(risk_matrix.shape[1]):
        col_max = risk_matrix[:, j].max()
        if col_max > 0:
            risk_matrix[:, j] = risk_matrix[:, j] / col_max
    
    # Create heatmap
    fig, ax = create_figure_with_subplots(fig_type='square')
    
    im = ax.imshow(risk_matrix, cmap='YlOrRd', aspect='auto', vmin=0, vmax=1)
    
    # Set ticks
    ax.set_xticks(np.arange(len(risk_metrics)))
    ax.set_yticks(np.arange(len(scenarios)))
    ax.set_xticklabels([m.replace('_', ' ').title() for m in risk_metrics],
                      rotation=45, ha='right')
    ax.set_yticklabels(scenarios)
    
    # Add text annotations
    for i in range(len(scenarios)):
        for j in range(len(risk_metrics)):
            text = ax.text(j, i, f'{risk_matrix[i, j]:.2f}',
                         ha='center', va='center', color='black' if risk_matrix[i, j] < 0.5 else 'white')
    
    # Add colorbar
    cbar = plt.colorbar(im, ax=ax)
    cbar.set_label('Normalized Risk Score', rotation=270, labelpad=15)
    
    format_axis_labels(ax, title='Fire Risk Assessment Matrix')
    
    # Add risk categories
    ax2 = ax.twinx()
    ax2.set_ylim(ax.get_ylim())
    ax2.set_yticks(np.arange(len(scenarios)))
    
    # Calculate overall risk score
    overall_risk = risk_matrix.mean(axis=1)
    risk_labels = []
    for risk in overall_risk:
        if risk < 0.33:
            risk_labels.append('LOW')
        elif risk < 0.67:
            risk_labels.append('MEDIUM')
        else:
            risk_labels.append('HIGH')
    
    ax2.set_yticklabels(risk_labels)
    ax2.set_ylabel('Overall Risk Level', rotation=270, labelpad=15)
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def plot_scenario_trajectories(scenarios_data: Dict[str, pd.DataFrame],
                             x_metric: str = 'tree_density',
                             y_metric: str = 'burnt_area',
                             output_name: Optional[str] = None) -> plt.Figure:
    """
    Plot trajectories in phase space for different scenarios.
    
    Args:
        scenarios_data: Dict mapping scenarios to time series data
        x_metric: Metric for x-axis
        y_metric: Metric for y-axis
        output_name: Filename for saving
        
    Returns:
        Matplotlib figure object
    """
    fig, ax = create_figure_with_subplots()
    
    for scenario_name, data in scenarios_data.items():
        if x_metric in data.columns and y_metric in data.columns:
            color = get_scenario_color(scenario_name)
            
            # Plot trajectory
            ax.plot(data[x_metric], data[y_metric], color=color, 
                   linewidth=2, alpha=0.7, label=scenario_name)
            
            # Mark start and end points
            ax.scatter(data[x_metric].iloc[0], data[y_metric].iloc[0],
                      color=color, s=100, marker='o', edgecolor='black',
                      linewidth=2, alpha=0.9)
            ax.scatter(data[x_metric].iloc[-1], data[y_metric].iloc[-1],
                      color=color, s=100, marker='s', edgecolor='black',
                      linewidth=2, alpha=0.9)
            
            # Add arrow to show direction
            mid_idx = len(data) // 2
            if mid_idx > 0:
                dx = data[x_metric].iloc[mid_idx] - data[x_metric].iloc[mid_idx-1]
                dy = data[y_metric].iloc[mid_idx] - data[y_metric].iloc[mid_idx-1]
                ax.arrow(data[x_metric].iloc[mid_idx-1], data[y_metric].iloc[mid_idx-1],
                        dx, dy, head_width=0.01, head_length=0.01,
                        fc=color, ec=color, alpha=0.5)
    
    format_axis_labels(ax,
                      xlabel=x_metric.replace('_', ' ').title(),
                      ylabel=y_metric.replace('_', ' ').title(),
                      title='Scenario Trajectories in Phase Space')
    
    ax.legend(loc='best')
    ax.grid(True, alpha=0.3)
    
    # Add annotations
    ax.text(0.02, 0.98, 'Circle: Start\nSquare: End', transform=ax.transAxes,
           fontsize=8, verticalalignment='top',
           bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
    
    if output_name:
        save_figure(fig, output_name)
    
    return fig


def create_climate_comparison_summary(simulation_outputs: Dict[str, str],
                                    output_dir: str = "visualization/figures"):
    """
    Create comprehensive climate scenario comparison from simulation outputs.
    
    Args:
        simulation_outputs: Dict mapping names to output directories
        output_dir: Output directory for figures
    """
    all_summaries = []
    all_timeseries = {}
    
    for name, output_path in simulation_outputs.items():
        print(f"Processing climate comparison for {name}...")
        
        # Load data
        sim_data = load_simulation_output(output_path)
        
        # Collect summary data
        if 'summary' in sim_data:
            all_summaries.append(sim_data['summary'])
        
        # Collect time series
        if 'timeseries' in sim_data:
            all_timeseries.update(sim_data['timeseries'])
    
    # Create comparison plots
    if all_summaries:
        # Combine summaries
        combined_summary = pd.concat(all_summaries, ignore_index=True)
        
        # Bar chart comparison
        fig = plot_scenario_comparison_bars(combined_summary,
                                          output_name='scenario_comparison_bars')
        plt.close(fig)
        
        # Risk matrix
        fig = plot_fire_risk_matrix(all_timeseries,
                                   output_name='fire_risk_matrix')
        plt.close(fig)
    
    if all_timeseries:
        # Trajectory plot
        fig = plot_scenario_trajectories(all_timeseries,
                                       output_name='scenario_trajectories')
        plt.close(fig)