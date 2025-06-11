"""
Color schemes and palettes for forest fire visualizations.
"""
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from typing import Dict, List, Tuple, Optional


# Cell state colors
CELL_STATE_COLORS = {
    'Empty': '#E0E0E0',      # Light gray
    'Tree': '#228B22',       # Forest green
    'Burning': '#FF4500',    # Orange red
    'Burnt': '#2F4F4F'       # Dark slate gray
}

# Numeric state mapping
STATE_NUMERIC_MAP = {
    'Empty': 0,
    'Tree': 1,
    'Burning': 2,
    'Burnt': 3
}

# Vegetation type colors
VEGETATION_COLORS = {
    'DenseForest': '#006400',     # Dark green
    'SparseForest': '#228B22',    # Forest green
    'Shrubland': '#8B7355',       # Saddle brown
    'Grassland': '#9ACD32',       # Yellow green
    'Barren': '#D2B48C'           # Tan
}

# Phase colors
PHASE_COLORS = {
    'SubCritical': '#4169E1',     # Royal blue
    'Critical': '#FFD700',        # Gold
    'SuperCritical': '#DC143C'    # Crimson
}

# Climate scenario colors
SCENARIO_COLORS = {
    'baseline': '#2E8B57',        # Sea green
    'RCP2.6': '#4682B4',         # Steel blue
    'RCP4.5': '#FF8C00',         # Dark orange
    'RCP8.5': '#DC143C'          # Crimson
}

# Colormaps
PHASE_COLORMAP = 'RdYlBu_r'      # Blue -> Yellow -> Red for phase transitions
TERRAIN_COLORMAP = 'terrain'     # For elevation data
MOISTURE_COLORMAP = 'Blues'      # For moisture content
TEMPERATURE_COLORMAP = 'hot'     # For temperature data
FIRE_INTENSITY_COLORMAP = 'YlOrRd'  # Yellow -> Orange -> Red for fire intensity


def create_state_colormap() -> mcolors.ListedColormap:
    """
    Create discrete colormap for cell states.
    
    Returns:
        ListedColormap for cell states
    """
    colors = [CELL_STATE_COLORS['Empty'],
              CELL_STATE_COLORS['Tree'],
              CELL_STATE_COLORS['Burning'],
              CELL_STATE_COLORS['Burnt']]
    
    cmap = mcolors.ListedColormap(colors, name='cell_states')
    bounds = [0, 1, 2, 3, 4]
    norm = mcolors.BoundaryNorm(bounds, cmap.N)
    
    return cmap, norm


def create_vegetation_colormap() -> mcolors.ListedColormap:
    """
    Create discrete colormap for vegetation types.
    
    Returns:
        ListedColormap for vegetation types
    """
    colors = list(VEGETATION_COLORS.values())
    cmap = mcolors.ListedColormap(colors, name='vegetation')
    
    return cmap


def create_phase_colormap(n_levels: int = 100) -> mcolors.LinearSegmentedColormap:
    """
    Create smooth colormap for phase diagrams.
    
    Args:
        n_levels: Number of color levels
        
    Returns:
        LinearSegmentedColormap for phase transitions
    """
    colors = ['#4169E1', '#87CEEB', '#FFD700', '#FF8C00', '#DC143C']
    positions = [0.0, 0.25, 0.5, 0.75, 1.0]
    
    cmap = mcolors.LinearSegmentedColormap.from_list(
        'phase_transition', list(zip(positions, colors)), N=n_levels
    )
    
    return cmap


def create_fire_intensity_colormap() -> mcolors.LinearSegmentedColormap:
    """
    Create colormap for fire intensity visualization.
    
    Returns:
        LinearSegmentedColormap for fire intensity
    """
    colors = ['#FFFFFF', '#FFFF00', '#FFA500', '#FF4500', '#8B0000']
    positions = [0.0, 0.2, 0.5, 0.8, 1.0]
    
    cmap = mcolors.LinearSegmentedColormap.from_list(
        'fire_intensity', list(zip(positions, colors))
    )
    
    return cmap


def get_scenario_color(scenario: str) -> str:
    """
    Get color for climate scenario.
    
    Args:
        scenario: Scenario name
        
    Returns:
        Hex color string
    """
    # Handle case variations
    scenario_lower = scenario.lower().replace(' ', '')
    for key, color in SCENARIO_COLORS.items():
        if key.lower() in scenario_lower:
            return color
    
    # Default color if not found
    return '#808080'  # Gray


def create_diverging_colormap(center_value: float = 0.5,
                             low_color: str = '#4169E1',
                             mid_color: str = '#FFFFFF', 
                             high_color: str = '#DC143C') -> Tuple[mcolors.Colormap, mcolors.Normalize]:
    """
    Create diverging colormap centered at specific value.
    
    Args:
        center_value: Value to center colormap at
        low_color: Color for low values
        mid_color: Color for center value
        high_color: Color for high values
        
    Returns:
        Tuple of (colormap, normalization)
    """
    cmap = mcolors.LinearSegmentedColormap.from_list(
        'diverging', [low_color, mid_color, high_color]
    )
    
    # Create normalization that centers at specified value
    norm = mcolors.TwoSlopeNorm(vmin=0, vcenter=center_value, vmax=1)
    
    return cmap, norm


def apply_transparency_gradient(colors: List[str], 
                               alpha_start: float = 0.2,
                               alpha_end: float = 1.0) -> List[Tuple[float, float, float, float]]:
    """
    Apply transparency gradient to list of colors.
    
    Args:
        colors: List of color strings
        alpha_start: Starting alpha value
        alpha_end: Ending alpha value
        
    Returns:
        List of RGBA tuples
    """
    rgba_colors = []
    alphas = np.linspace(alpha_start, alpha_end, len(colors))
    
    for color, alpha in zip(colors, alphas):
        rgb = mcolors.to_rgb(color)
        rgba_colors.append((*rgb, alpha))
    
    return rgba_colors


def create_qualitative_palette(n_colors: int = 8) -> List[str]:
    """
    Create qualitative color palette for categorical data.
    
    Args:
        n_colors: Number of colors needed
        
    Returns:
        List of hex color strings
    """
    if n_colors <= 8:
        # Use ColorBrewer Set2
        colors = ['#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3',
                 '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3']
    else:
        # Generate using HSV space
        hues = np.linspace(0, 1, n_colors, endpoint=False)
        colors = [mcolors.hsv_to_rgb([h, 0.7, 0.9]) for h in hues]
        colors = [mcolors.to_hex(c) for c in colors]
    
    return colors[:n_colors]


def get_colorbar_label(data_type: str) -> str:
    """
    Get appropriate colorbar label for data type.
    
    Args:
        data_type: Type of data being visualized
        
    Returns:
        Formatted label string
    """
    labels = {
        'burnt_fraction': 'Burnt Fraction',
        'percolation_indicator': 'Percolation Indicator',
        'tree_density': 'Tree Density',
        'moisture': 'Moisture Content',
        'temperature': 'Temperature (°C)',
        'elevation': 'Elevation (m)',
        'fire_intensity': 'Fire Intensity',
        'cluster_size': 'Cluster Size',
        'correlation': 'Correlation',
        'phase': 'Phase'
    }
    
    return labels.get(data_type, data_type.replace('_', ' ').title())


def create_custom_legend(ax, color_dict: Dict[str, str], 
                        title: Optional[str] = None,
                        loc: str = 'best',
                        ncol: int = 1):
    """
    Create custom legend from color dictionary.
    
    Args:
        ax: Matplotlib axes object
        color_dict: Dictionary mapping labels to colors
        title: Legend title
        loc: Legend location
        ncol: Number of columns
    """
    from matplotlib.patches import Patch
    
    elements = [Patch(facecolor=color, label=label) 
               for label, color in color_dict.items()]
    
    legend = ax.legend(handles=elements, title=title, loc=loc, 
                      ncol=ncol, frameon=True, fancybox=True,
                      edgecolor='gray', framealpha=0.9)
    
    return legend