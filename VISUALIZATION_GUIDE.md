# Visualization Guide for Forest Fire Simulator

## Overview
The CLI now includes a `visualize` command that lets you launch different visualization tools for your simulation results.

## Available Visualization Types

### 1. Web Viewer (Default)
Interactive web-based visualization with real-time animation.

```bash
# Launch web viewer on default port 8080
forest-fire-simulator visualize --input output/simulation.json

# Use a different port
forest-fire-simulator visualize --input output/simulation.json --port 3000

# Explicitly specify web viewer
forest-fire-simulator visualize --type web --input output/simulation.json
```

**Features:**
- 🎬 Animated fire spread playback
- 📊 Real-time metrics display
- 🗺️ Terrain and vegetation layers
- ⏯️ Playback controls (play/pause/speed)
- 🖱️ Interactive zoom and pan

### 2. 3D Viewer
Three-dimensional visualization using VTK format.

```bash
# Launch 3D viewer
forest-fire-simulator visualize --type 3d --input output/simulation.json
```

**Features:**
- 🏔️ 3D terrain elevation
- 🔥 Fire intensity as color gradient
- 💨 Wind direction vectors
- 📷 Camera controls (rotate/zoom/pan)
- ⏰ Time-based animation

**Note:** Requires a VTK viewer like ParaView installed on your system.

### 3. Jupyter Notebook
Interactive analysis notebook with Python/Plotly.

```bash
# Create and launch Jupyter notebook
forest-fire-simulator visualize --type notebook --input output/simulation.json
```

**Features:**
- 📈 Interactive plots with Plotly
- 📊 Statistical analysis tools
- 🔄 Phase transition diagrams
- 🐍 Custom Python analysis code
- 📓 Reproducible analysis workflow

**Note:** Requires Jupyter installed (`pip install jupyter plotly pandas numpy`).

## Complete Workflow Example

```bash
# 1. Run a simulation
forest-fire-simulator run --region wallis --scenario rcp45 --year 2050 --steps 1000

# 2. Launch web viewer to see the animation
forest-fire-simulator visualize --input output/simulation.json

# 3. Export to VTK for 3D analysis
forest-fire-simulator export --input output/simulation.json --format vtk --output output/viz.vtk

# 4. Open 3D viewer
forest-fire-simulator visualize --type 3d --input output/viz.vtk

# 5. Create analysis notebook
forest-fire-simulator visualize --type notebook --input output/simulation.json
```

## Web Viewer Details

The web viewer serves your visualization locally and opens it in your default browser. The visualization includes:

1. **Main Display**: Grid showing fire spread animation
2. **Control Panel**: 
   - Play/Pause button
   - Speed control slider
   - Time step display
3. **Metrics Panel**: Real-time display of:
   - Active fires count
   - Total burnt area
   - Tree density
   - Percolation indicator
4. **Layer Controls**: Toggle visibility of:
   - Fire state
   - Terrain elevation
   - Vegetation type
   - Wind vectors

## Tips

1. **Performance**: For large simulations (>500x500 grid), the web viewer may be slow. Consider using the 3D viewer or notebook for analysis.

2. **Multiple Views**: You can run multiple visualizations simultaneously on different ports:
   ```bash
   forest-fire-simulator visualize --port 8080 &
   forest-fire-simulator visualize --port 8081 --input other_simulation.json &
   ```

3. **Export Frames**: To create a video, first export frames:
   ```bash
   forest-fire-simulator export --input simulation.json --format frames --output frames/
   # Then use ffmpeg or similar to create video
   ffmpeg -r 30 -i frames/frame_%04d.png -c:v libx264 -vf fps=30 -pix_fmt yuv420p simulation.mp4
   ```

## Implementation Notes

Currently, the visualization command:
- ✅ Creates the command structure and parsing
- ✅ Provides placeholders for launching different viewers
- ✅ Generates Jupyter notebooks automatically
- ⚠️ Web server requires http4s (not yet added to dependencies)
- ⚠️ 3D viewer integration needs system-specific launcher

To fully implement:
1. Add http4s dependencies for web server
2. Include web viewer HTML/JS files as resources
3. Add VTK export if not already present
4. Create system-specific launchers for external tools