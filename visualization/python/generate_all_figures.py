#!/usr/bin/env python3
"""
Master script to generate all visualizations for forest fire simulations.
Processes simulation output files and creates comprehensive figure sets.
"""
import os
import sys
import argparse
from pathlib import Path
from typing import Dict, List, Optional
import warnings

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# Import visualization modules
from utils.data_loader import load_simulation_output, load_all_snapshots
from phase_diagrams import create_phase_diagram_summary
from time_series_analysis import create_time_series_summary
from spatial_patterns import create_spatial_summary, create_animation_frames
from climate_comparison import create_climate_comparison_summary
from terrain_3d_visualization import create_3d_summary


def find_simulation_outputs(base_dir: str = "output") -> Dict[str, str]:
    """
    Find all simulation output directories.
    
    Args:
        base_dir: Base directory to search
        
    Returns:
        Dict mapping simulation names to output directories
    """
    outputs = {}
    base_path = Path(base_dir)
    
    if not base_path.exists():
        warnings.warn(f"Output directory '{base_dir}' not found")
        return outputs
    
    # Look for directories containing CSV files
    for subdir in base_path.iterdir():
        if subdir.is_dir():
            csv_files = list(subdir.glob("*.csv"))
            if csv_files:
                outputs[subdir.name] = str(subdir)
    
    return outputs


def generate_all_visualizations(output_dirs: Dict[str, str],
                              figure_dir: str = "visualization/figures",
                              selected_analyses: Optional[List[str]] = None):
    """
    Generate all visualization types from simulation outputs.
    
    Args:
        output_dirs: Dict mapping names to output directories
        figure_dir: Directory to save figures
        selected_analyses: List of analyses to run (if None, run all)
    """
    # Create figure directory
    Path(figure_dir).mkdir(parents=True, exist_ok=True)
    
    available_analyses = {
        'phase': 'Phase Diagrams',
        'timeseries': 'Time Series Analysis',
        'spatial': 'Spatial Patterns',
        'climate': 'Climate Comparisons',
        '3d': '3D Terrain Visualization',
        'animation': 'Animation Frames'
    }
    
    # Determine which analyses to run
    if selected_analyses:
        analyses_to_run = [a for a in selected_analyses if a in available_analyses]
    else:
        analyses_to_run = list(available_analyses.keys())
    
    print(f"\nGenerating visualizations for {len(output_dirs)} simulation(s)...")
    print(f"Analyses to run: {', '.join(analyses_to_run)}\n")
    
    # Phase diagram analysis
    if 'phase' in analyses_to_run:
        print("\n=== Generating Phase Diagrams ===")
        phase_files = {}
        
        for name, output_dir in output_dirs.items():
            phase_data_files = list(Path(output_dir).glob("phase_data*.csv"))
            for phase_file in phase_data_files:
                phase_files[f"{name}_{phase_file.stem}"] = str(phase_file)
        
        if phase_files:
            create_phase_diagram_summary(phase_files, figure_dir)
        else:
            print("No phase data files found")
    
    # Time series analysis
    if 'timeseries' in analyses_to_run:
        print("\n=== Generating Time Series Plots ===")
        create_time_series_summary(output_dirs, figure_dir)
    
    # Spatial pattern analysis
    if 'spatial' in analyses_to_run:
        print("\n=== Generating Spatial Pattern Analysis ===")
        
        for name, output_dir in output_dirs.items():
            # Load final grid snapshots
            grid_files = list(Path(output_dir).glob("*_grid.csv"))
            
            if grid_files:
                # Use the most recent snapshot
                grid_file = sorted(grid_files)[-1]
                sim_data = load_simulation_output(output_dir)
                
                if 'grid_snapshots' in sim_data:
                    create_spatial_summary(sim_data['grid_snapshots'], figure_dir)
            else:
                print(f"No grid snapshots found for {name}")
    
    # Climate comparison
    if 'climate' in analyses_to_run:
        print("\n=== Generating Climate Comparisons ===")
        create_climate_comparison_summary(output_dirs, figure_dir)
    
    # 3D terrain visualization
    if '3d' in analyses_to_run:
        print("\n=== Generating 3D Terrain Visualizations ===")
        
        for name, output_dir in output_dirs.items():
            sim_data = load_simulation_output(output_dir)
            
            if 'grid_snapshots' in sim_data:
                # Check if elevation data exists
                first_snapshot = next(iter(sim_data['grid_snapshots'].values()))
                if 'elevation' in first_snapshot.columns:
                    create_3d_summary(sim_data['grid_snapshots'], figure_dir)
                else:
                    print(f"No elevation data found for {name}, skipping 3D visualization")
    
    # Animation frames
    if 'animation' in analyses_to_run:
        print("\n=== Generating Animation Frames ===")
        
        for name, output_dir in output_dirs.items():
            # Load all grid snapshots for animation
            snapshots = load_all_snapshots(output_dir, pattern="*_grid.csv")
            
            if len(snapshots) > 1:
                print(f"Creating animation for {name} with {len(snapshots)} frames...")
                animation_path = create_animation_frames(snapshots, figure_dir, 
                                                       fps=10, show_metrics=True)
                print(f"Animation saved to: {animation_path}")
            else:
                print(f"Not enough snapshots for animation in {name}")
    
    print(f"\n✓ All visualizations generated in: {figure_dir}")


def generate_sample_data(output_dir: str = "output/sample"):
    """
    Generate sample data for testing visualizations.
    """
    import numpy as np
    import pandas as pd
    
    print("Generating sample data for testing...")
    
    # Create output directory
    Path(output_dir).mkdir(parents=True, exist_ok=True)
    
    # Generate sample time series data
    time = np.linspace(0, 100, 200)
    ts_data = pd.DataFrame({
        'time': time,
        'active_fires': np.maximum(0, 50 * np.sin(time/10) + np.random.normal(0, 5, len(time))),
        'burnt_area': np.cumsum(np.maximum(0, np.random.normal(2, 1, len(time)))),
        'percolation_indicator': 1 / (1 + np.exp(-0.1 * (time - 50))) + np.random.normal(0, 0.05, len(time)),
        'tree_density': 0.7 - 0.002 * time + np.random.normal(0, 0.01, len(time)),
        'largest_cluster': np.maximum(1, 20 * np.sin(time/15) ** 2 + np.random.normal(0, 3, len(time)))
    })
    ts_data.to_csv(f"{output_dir}/baseline_timeseries.csv", index=False)
    
    # Generate sample grid data
    grid_size = 50
    x, y = np.meshgrid(range(grid_size), range(grid_size))
    
    # Create realistic elevation using sine waves
    elevation = 500 + 50 * np.sin(x/10) + 30 * np.cos(y/8) + np.random.normal(0, 5, x.shape)
    
    # Create fire pattern
    fire_center = (25, 25)
    distance = np.sqrt((x - fire_center[0])**2 + (y - fire_center[1])**2)
    
    grid_data = []
    for i in range(grid_size):
        for j in range(grid_size):
            dist = distance[i, j]
            
            # Determine state based on distance from fire center
            if dist < 5:
                state = 'Burnt'
            elif dist < 8:
                state = 'Burning'
            elif dist < 20:
                state = 'Tree'
            else:
                state = 'Tree' if np.random.random() > 0.1 else 'Empty'
            
            # Assign vegetation type based on elevation
            elev = elevation[i, j]
            if elev > 550:
                vegetation = 'SparseForest'
            elif elev > 520:
                vegetation = 'DenseForest'
            else:
                vegetation = 'Grassland'
            
            grid_data.append({
                'x': j,
                'y': i,
                'state': state,
                'elevation': elev,
                'vegetation': vegetation,
                'moisture': 0.3 + 0.2 * np.random.random(),
                'temperature': 25 + 5 * np.random.random()
            })
    
    grid_df = pd.DataFrame(grid_data)
    grid_df.to_csv(f"{output_dir}/baseline_final_grid.csv", index=False)
    
    # Generate phase data
    phase_data = []
    for temp in np.linspace(10, 40, 10):
        for density in np.linspace(0, 1, 10):
            burnt_frac = 1 / (1 + np.exp(-0.5 * (density - 0.5))) * (1 + 0.02 * (temp - 25))
            phase_data.append({
                'temperature': temp,
                'tree_density': density,
                'burnt_fraction': np.clip(burnt_frac + np.random.normal(0, 0.05), 0, 1),
                'percolation': 1 if burnt_frac > 0.5 else 0
            })
    
    phase_df = pd.DataFrame(phase_data)
    phase_df.to_csv(f"{output_dir}/phase_data_density_temperature.csv", index=False)
    
    # Generate comparison summary
    summary_data = pd.DataFrame([
        {'scenario': 'baseline', 'total_burnt_area': 245, 'burn_duration': 48.5, 
         'max_cluster_size': 120, 'percolated': False},
        {'scenario': 'RCP2.6', 'total_burnt_area': 280, 'burn_duration': 45.2,
         'max_cluster_size': 150, 'percolated': False},
        {'scenario': 'RCP4.5', 'total_burnt_area': 350, 'burn_duration': 41.8,
         'max_cluster_size': 220, 'percolated': True},
        {'scenario': 'RCP8.5', 'total_burnt_area': 450, 'burn_duration': 35.7,
         'max_cluster_size': 350, 'percolated': True}
    ])
    summary_data.to_csv(f"{output_dir}/comparison_summary.csv", index=False)
    
    print(f"Sample data generated in: {output_dir}")


def main():
    """Main entry point for the visualization generator."""
    parser = argparse.ArgumentParser(
        description="Generate visualizations for forest fire simulations"
    )
    parser.add_argument(
        '--output-dir', '-o',
        default='output',
        help='Base directory containing simulation outputs (default: output)'
    )
    parser.add_argument(
        '--figure-dir', '-f',
        default='visualization/figures',
        help='Directory to save generated figures (default: visualization/figures)'
    )
    parser.add_argument(
        '--analyses', '-a',
        nargs='+',
        choices=['phase', 'timeseries', 'spatial', 'climate', '3d', 'animation'],
        help='Specific analyses to run (default: all)'
    )
    parser.add_argument(
        '--generate-sample', '-s',
        action='store_true',
        help='Generate sample data for testing'
    )
    parser.add_argument(
        '--simulation', '-sim',
        help='Process only a specific simulation by name'
    )
    
    args = parser.parse_args()
    
    # Generate sample data if requested
    if args.generate_sample:
        generate_sample_data()
        # Use sample data for visualization
        output_dirs = {'sample': 'output/sample'}
    else:
        # Find simulation outputs
        output_dirs = find_simulation_outputs(args.output_dir)
        
        if not output_dirs:
            print(f"No simulation outputs found in '{args.output_dir}'")
            print("Use --generate-sample to create test data")
            return
        
        # Filter by specific simulation if requested
        if args.simulation:
            if args.simulation in output_dirs:
                output_dirs = {args.simulation: output_dirs[args.simulation]}
            else:
                print(f"Simulation '{args.simulation}' not found")
                print(f"Available: {', '.join(output_dirs.keys())}")
                return
    
    print(f"Found {len(output_dirs)} simulation output(s):")
    for name, path in output_dirs.items():
        print(f"  - {name}: {path}")
    
    # Generate visualizations
    try:
        generate_all_visualizations(output_dirs, args.figure_dir, args.analyses)
        print("\n✓ Visualization generation complete!")
    except Exception as e:
        print(f"\n✗ Error during visualization: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()