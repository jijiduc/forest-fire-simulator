"""
Data loading utilities for forest fire simulation CSV outputs.
"""
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Tuple, Dict, List, Optional


def load_timeseries(filepath: str) -> pd.DataFrame:
    """
    Load time series data from CSV file.
    
    Args:
        filepath: Path to timeseries CSV file
        
    Returns:
        DataFrame with time series data, time column as index
    """
    df = pd.read_csv(filepath)
    if 'time' in df.columns:
        df.set_index('time', inplace=True)
    return df


def load_grid_snapshot(filepath: str) -> pd.DataFrame:
    """
    Load grid snapshot data from CSV file.
    
    Args:
        filepath: Path to grid snapshot CSV file
        
    Returns:
        DataFrame with grid data, MultiIndex on (x, y)
    """
    df = pd.read_csv(filepath)
    if 'x' in df.columns and 'y' in df.columns:
        df.set_index(['x', 'y'], inplace=True)
    return df


def load_phase_data(filepath: str) -> pd.DataFrame:
    """
    Load phase transition data from CSV file.
    
    Args:
        filepath: Path to phase data CSV file
        
    Returns:
        DataFrame with phase transition data
    """
    return pd.read_csv(filepath)


def load_climate_scenarios(filepath: str) -> pd.DataFrame:
    """
    Load climate scenario data from CSV file.
    
    Args:
        filepath: Path to climate scenarios CSV file
        
    Returns:
        DataFrame with climate scenario data
    """
    return pd.read_csv(filepath)


def load_comparison_summary(filepath: str) -> pd.DataFrame:
    """
    Load comparison summary data from CSV file.
    
    Args:
        filepath: Path to comparison summary CSV file
        
    Returns:
        DataFrame with comparison summary data
    """
    df = pd.read_csv(filepath)
    # Convert boolean strings to actual booleans
    if 'percolated' in df.columns:
        df['percolated'] = df['percolated'].map({'true': True, 'false': False})
    return df


def infer_grid_dimensions(grid_df: pd.DataFrame) -> Tuple[int, int]:
    """
    Infer grid dimensions from grid snapshot DataFrame.
    
    Args:
        grid_df: DataFrame with x, y coordinates
        
    Returns:
        Tuple of (width, height)
    """
    if isinstance(grid_df.index, pd.MultiIndex):
        x_vals = grid_df.index.get_level_values('x')
        y_vals = grid_df.index.get_level_values('y')
    else:
        x_vals = grid_df['x']
        y_vals = grid_df['y']
    
    width = int(x_vals.max()) + 1
    height = int(y_vals.max()) + 1
    return width, height


def grid_to_matrix(grid_df: pd.DataFrame, value_column: str) -> np.ndarray:
    """
    Convert grid DataFrame to 2D numpy array.
    
    Args:
        grid_df: DataFrame with grid data
        value_column: Column name to use for values
        
    Returns:
        2D numpy array with grid values
    """
    width, height = infer_grid_dimensions(grid_df)
    matrix = np.full((height, width), np.nan)
    
    if isinstance(grid_df.index, pd.MultiIndex):
        for (x, y), row in grid_df.iterrows():
            matrix[int(y), int(x)] = row[value_column]
    else:
        for _, row in grid_df.iterrows():
            matrix[int(row['y']), int(row['x'])] = row[value_column]
    
    return matrix


def load_all_snapshots(directory: str, pattern: str = "*_grid.csv") -> Dict[str, pd.DataFrame]:
    """
    Load all grid snapshots from a directory.
    
    Args:
        directory: Directory containing grid snapshot files
        pattern: Glob pattern for snapshot files
        
    Returns:
        Dictionary mapping filename to DataFrame
    """
    snapshots = {}
    path = Path(directory)
    
    for file in sorted(path.glob(pattern)):
        snapshots[file.stem] = load_grid_snapshot(str(file))
    
    return snapshots


def load_simulation_output(output_dir: str) -> Dict[str, pd.DataFrame]:
    """
    Load all simulation output files from a directory.
    
    Args:
        output_dir: Directory containing simulation output
        
    Returns:
        Dictionary with keys: 'timeseries', 'grid_snapshots', 'summary'
    """
    output_path = Path(output_dir)
    data = {}
    
    # Load time series data
    timeseries_files = list(output_path.glob("*_timeseries.csv"))
    if timeseries_files:
        data['timeseries'] = {f.stem: load_timeseries(str(f)) for f in timeseries_files}
    
    # Load grid snapshots
    data['grid_snapshots'] = load_all_snapshots(output_dir)
    
    # Load summary data
    summary_file = output_path / "comparison_summary.csv"
    if summary_file.exists():
        data['summary'] = load_comparison_summary(str(summary_file))
    
    return data


def create_phase_grid(phase_data: pd.DataFrame, 
                     x_param: str, 
                     y_param: str, 
                     value_param: str) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Create 2D grid for phase diagram plotting.
    
    Args:
        phase_data: DataFrame with phase transition data
        x_param: Column name for x-axis parameter
        y_param: Column name for y-axis parameter
        value_param: Column name for values (e.g., 'burnt_fraction')
        
    Returns:
        Tuple of (X grid, Y grid, Z values)
    """
    # Get unique parameter values
    x_unique = sorted(phase_data[x_param].unique())
    y_unique = sorted(phase_data[y_param].unique())
    
    # Create meshgrid
    X, Y = np.meshgrid(x_unique, y_unique)
    
    # Create value matrix
    Z = np.full(X.shape, np.nan)
    
    for i, y_val in enumerate(y_unique):
        for j, x_val in enumerate(x_unique):
            mask = (phase_data[x_param] == x_val) & (phase_data[y_param] == y_val)
            if mask.any():
                Z[i, j] = phase_data.loc[mask, value_param].iloc[0]
    
    return X, Y, Z


def extract_state_matrix(grid_df: pd.DataFrame) -> np.ndarray:
    """
    Extract cell state as numeric matrix for visualization.
    
    Args:
        grid_df: DataFrame with grid data including 'state' column
        
    Returns:
        2D numpy array with numeric state values
    """
    state_map = {
        'Empty': 0,
        'Tree': 1,
        'Burning': 2,
        'Burnt': 3
    }
    
    width, height = infer_grid_dimensions(grid_df)
    matrix = np.zeros((height, width))
    
    if isinstance(grid_df.index, pd.MultiIndex):
        for (x, y), row in grid_df.iterrows():
            matrix[int(y), int(x)] = state_map.get(row['state'], 0)
    else:
        for _, row in grid_df.iterrows():
            matrix[int(row['y']), int(row['x'])] = state_map.get(row['state'], 0)
    
    return matrix