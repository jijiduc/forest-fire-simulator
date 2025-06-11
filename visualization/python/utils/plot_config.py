"""
Plotting configuration for publication-quality figures.
"""
import matplotlib.pyplot as plt
import matplotlib as mpl
from pathlib import Path
from typing import Tuple, List, Optional


def set_publication_style():
    """
    Set matplotlib parameters for publication-quality figures.
    """
    # Font settings
    plt.rcParams['font.family'] = 'sans-serif'
    plt.rcParams['font.sans-serif'] = ['Arial', 'Helvetica', 'DejaVu Sans']
    plt.rcParams['font.size'] = 10
    plt.rcParams['axes.labelsize'] = 11
    plt.rcParams['axes.titlesize'] = 12
    plt.rcParams['xtick.labelsize'] = 9
    plt.rcParams['ytick.labelsize'] = 9
    plt.rcParams['legend.fontsize'] = 9
    
    # Line and marker settings
    plt.rcParams['lines.linewidth'] = 1.5
    plt.rcParams['lines.markersize'] = 6
    plt.rcParams['patch.linewidth'] = 1.0
    
    # Axes settings
    plt.rcParams['axes.linewidth'] = 1.0
    plt.rcParams['axes.spines.top'] = False
    plt.rcParams['axes.spines.right'] = False
    plt.rcParams['xtick.major.width'] = 1.0
    plt.rcParams['ytick.major.width'] = 1.0
    plt.rcParams['xtick.major.size'] = 4
    plt.rcParams['ytick.major.size'] = 4
    
    # Grid settings
    plt.rcParams['axes.grid'] = True
    plt.rcParams['grid.alpha'] = 0.3
    plt.rcParams['grid.linestyle'] = '--'
    
    # Figure settings
    plt.rcParams['figure.dpi'] = 100
    plt.rcParams['savefig.dpi'] = 300
    plt.rcParams['savefig.bbox'] = 'tight'
    plt.rcParams['savefig.pad_inches'] = 0.1
    
    # Color settings
    plt.rcParams['image.cmap'] = 'viridis'
    
    # Math text
    plt.rcParams['mathtext.fontset'] = 'stix'


def get_figure_size(fig_type: str = 'default') -> Tuple[float, float]:
    """
    Get appropriate figure size for different plot types.
    
    Args:
        fig_type: Type of figure ('default', 'wide', 'tall', 'square', 'multi_panel')
        
    Returns:
        Tuple of (width, height) in inches
    """
    sizes = {
        'default': (6.5, 4.5),
        'wide': (8.0, 4.0),
        'tall': (5.0, 7.0),
        'square': (5.5, 5.5),
        'multi_panel': (7.5, 9.0),
        'phase_diagram': (6.0, 5.0),
        'time_series': (7.0, 6.0),
        'grid_view': (8.0, 6.0),
        '3d_view': (8.0, 6.0)
    }
    return sizes.get(fig_type, sizes['default'])


def save_figure(fig: plt.Figure, 
                name: str, 
                output_dir: str = "visualization/figures",
                formats: List[str] = None):
    """
    Save figure in multiple formats with consistent settings.
    
    Args:
        fig: Matplotlib figure object
        name: Base filename (without extension)
        output_dir: Output directory path
        formats: List of formats to save (default: ['png', 'pdf', 'svg'])
    """
    if formats is None:
        formats = ['png', 'pdf', 'svg']
    
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)
    
    for fmt in formats:
        filepath = output_path / f"{name}.{fmt}"
        fig.savefig(filepath, format=fmt, bbox_inches='tight', 
                   dpi=300 if fmt == 'png' else None)
        print(f"Saved: {filepath}")


def create_figure_with_subplots(n_rows: int = 1, 
                               n_cols: int = 1,
                               fig_type: str = 'default',
                               **subplot_kw) -> Tuple[plt.Figure, plt.Axes]:
    """
    Create figure with consistent subplot settings.
    
    Args:
        n_rows: Number of subplot rows
        n_cols: Number of subplot columns
        fig_type: Figure size type
        **subplot_kw: Additional subplot keywords
        
    Returns:
        Tuple of (figure, axes)
    """
    figsize = get_figure_size(fig_type)
    fig, axes = plt.subplots(n_rows, n_cols, figsize=figsize, **subplot_kw)
    
    # Adjust spacing
    plt.subplots_adjust(hspace=0.3, wspace=0.3)
    
    return fig, axes


def add_colorbar(im, ax, label: str = "", orientation: str = 'vertical', 
                pad: float = 0.05, fraction: float = 0.046):
    """
    Add a properly formatted colorbar to a plot.
    
    Args:
        im: Image/contour object
        ax: Axes to attach colorbar to
        label: Colorbar label
        orientation: 'vertical' or 'horizontal'
        pad: Space between plot and colorbar
        fraction: Fraction of axes size for colorbar
        
    Returns:
        Colorbar object
    """
    cbar = plt.colorbar(im, ax=ax, orientation=orientation, 
                       pad=pad, fraction=fraction)
    if label:
        cbar.set_label(label, rotation=270 if orientation == 'vertical' else 0,
                      labelpad=15 if orientation == 'vertical' else 10)
    
    # Adjust tick parameters
    cbar.ax.tick_params(labelsize=9, width=1, length=3)
    
    return cbar


def format_axis_labels(ax, xlabel: str = None, ylabel: str = None, 
                      title: str = None, grid: bool = True):
    """
    Apply consistent axis formatting.
    
    Args:
        ax: Matplotlib axes object
        xlabel: X-axis label
        ylabel: Y-axis label
        title: Plot title
        grid: Whether to show grid
    """
    if xlabel:
        ax.set_xlabel(xlabel)
    if ylabel:
        ax.set_ylabel(ylabel)
    if title:
        ax.set_title(title, pad=10)
    
    ax.grid(grid, alpha=0.3, linestyle='--')
    
    # Remove top and right spines
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)


def add_phase_boundaries(ax, x_critical: float = None, y_critical: float = None,
                        linestyle: str = '--', color: str = 'red', alpha: float = 0.7):
    """
    Add phase boundary lines to a plot.
    
    Args:
        ax: Matplotlib axes object
        x_critical: Critical value for vertical line
        y_critical: Critical value for horizontal line
        linestyle: Line style for boundaries
        color: Line color
        alpha: Line transparency
    """
    if x_critical is not None:
        ax.axvline(x_critical, linestyle=linestyle, color=color, 
                  alpha=alpha, label='Critical point')
    if y_critical is not None:
        ax.axhline(y_critical, linestyle=linestyle, color=color, 
                  alpha=alpha, label='Critical point' if x_critical is None else '')


def set_log_scale(ax, x_log: bool = False, y_log: bool = False):
    """
    Set logarithmic scale with proper formatting.
    
    Args:
        ax: Matplotlib axes object
        x_log: Use log scale for x-axis
        y_log: Use log scale for y-axis
    """
    if x_log:
        ax.set_xscale('log')
        ax.xaxis.set_major_locator(mpl.ticker.LogLocator(base=10, numticks=10))
        ax.xaxis.set_minor_locator(mpl.ticker.LogLocator(base=10, subs='auto', numticks=10))
    
    if y_log:
        ax.set_yscale('log')
        ax.yaxis.set_major_locator(mpl.ticker.LogLocator(base=10, numticks=10))
        ax.yaxis.set_minor_locator(mpl.ticker.LogLocator(base=10, subs='auto', numticks=10))


def add_annotation(ax, text: str, xy: Tuple[float, float], 
                  xytext: Optional[Tuple[float, float]] = None,
                  fontsize: int = 9, color: str = 'black',
                  bbox_props: Optional[dict] = None,
                  arrow_props: Optional[dict] = None):
    """
    Add formatted annotation to plot.
    
    Args:
        ax: Matplotlib axes object
        text: Annotation text
        xy: Point to annotate
        xytext: Text position (if different from xy)
        fontsize: Font size
        color: Text color
        bbox_props: Box properties dict
        arrow_props: Arrow properties dict
    """
    if bbox_props is None:
        bbox_props = dict(boxstyle="round,pad=0.3", facecolor="white", 
                         edgecolor="gray", alpha=0.8)
    
    if arrow_props is None and xytext is not None:
        arrow_props = dict(arrowstyle="->", color="gray", lw=1)
    
    ax.annotate(text, xy=xy, xytext=xytext or xy,
               fontsize=fontsize, color=color,
               bbox=bbox_props,
               arrowprops=arrow_props if xytext else None)


# Initialize style when module is imported
set_publication_style()