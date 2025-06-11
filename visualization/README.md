# Forest Fire Simulator Visualization Pipeline

## Overview
This Python visualization pipeline generates publication-quality figures from the forest fire simulation outputs. It includes 2D plots, phase diagrams, time series analysis, spatial pattern visualization, climate comparisons, and interactive 3D terrain visualizations.

## Installation

1. Create a virtual environment:
```bash
cd visualization
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
```

2. Install dependencies:
```bash
pip install -r requirements.txt
```

## Usage

### Generate visualizations from existing simulation output:
```bash
python python/generate_all_figures.py
```

### Generate sample data and test visualizations:
```bash
python python/generate_all_figures.py --generate-sample
```

### Run specific analyses only:
```bash
python python/generate_all_figures.py --analyses phase timeseries spatial
```

Available analysis types:
- `phase`: Phase diagrams and transition analysis
- `timeseries`: Time evolution plots  
- `spatial`: Grid states and cluster analysis
- `climate`: Climate scenario comparisons
- `3d`: 3D terrain visualizations
- `animation`: Animated fire spread

### Process specific simulation:
```bash
python python/generate_all_figures.py --simulation wallis_simulation
```

## Output Structure

Generated figures are saved in `visualization/figures/`:
- PNG format (300 DPI) for presentations
- PDF format for publications
- SVG format for editing
- HTML format for interactive 3D views

## Visualization Types

### 1. Phase Diagrams (`phase_diagrams.py`)
- 2D phase diagrams with control parameters
- Phase boundaries and critical points
- Susceptibility analysis
- Multi-scenario comparisons

### 2. Time Series Analysis (`time_series_analysis.py`)
- Multi-panel time evolution plots
- Smoothed trends and seasonal analysis
- Phase evolution visualization
- Cross-correlation analysis

### 3. Spatial Patterns (`spatial_patterns.py`)
- Grid state visualization with cell colors
- Cluster size distribution and analysis
- Spatial correlation functions
- Vegetation distribution analysis
- Animation frame generation

### 4. Climate Comparisons (`climate_comparison.py`)
- Scenario comparison bar charts
- Risk assessment matrices
- Phase space trajectories
- Statistical significance testing

### 5. 3D Terrain Visualization (`terrain_3d_visualization.py`)
- Interactive 3D terrain with fire overlay
- Cross-section views
- Multi-angle perspectives
- Animated fire spread on terrain

## Customization

### Color Schemes
Edit `utils/color_schemes.py` to modify:
- Cell state colors (Empty, Tree, Burning, Burnt)
- Vegetation type colors
- Climate scenario colors
- Phase diagram colormaps

### Plot Styling
Edit `utils/plot_config.py` to adjust:
- Figure sizes and DPI
- Font sizes and styles
- Grid and axis properties
- Publication-specific formatting

### Data Loading
The `utils/data_loader.py` module handles various CSV formats:
- Time series data
- Grid snapshots
- Phase transition data
- Climate scenarios
- Comparison summaries

## Example Workflow

1. Run a simulation in the main project:
```bash
sbt "run --scenario baseline --steps 1000"
```

2. Generate all visualizations:
```bash
cd visualization
source venv/bin/activate
python python/generate_all_figures.py
```

3. View results:
```bash
# Open PNG files
xdg-open figures/phase_diagram_*.png

# Open interactive 3D visualizations
firefox terrain_3d_*.html
```

## Troubleshooting

### Missing dependencies
If you get import errors, ensure the virtual environment is activated and all dependencies are installed:
```bash
source venv/bin/activate
pip install -r requirements.txt
```

### No output found
Make sure simulation data exists in the `output/` directory. Use `--generate-sample` to create test data.

### 3D visualization issues
- Interactive HTML files require a modern web browser
- Static 3D images require the `kaleido` package (included in requirements.txt)

### Memory issues with large grids
For very large simulations, consider:
- Processing snapshots individually
- Reducing animation frame rate
- Using data decimation for visualizations