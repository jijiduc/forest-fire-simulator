# Forest Fire Simulation Web Viewer

Interactive web-based viewer for visualizing forest fire simulation results.

## Features

### Core Functionality
- **2D Grid Visualization**: Canvas-based rendering with pan and zoom controls
- **3D Terrain View**: Three.js-based 3D visualization with elevation data support
- **Playback Controls**: Play/pause, step forward/backward, speed control
- **Real-time Metrics**: Display of active fires, burnt area, percolation indicators
- **Timeline Scrubber**: Jump to any point in the simulation

### Interactive Features
- **Drag & Drop**: Drop JSON simulation files directly onto the page
- **Keyboard Shortcuts**:
  - `Space`: Play/Pause
  - `←/→`: Step frames
  - `F`: Fullscreen mode
- **Mouse Controls**:
  - Drag to pan
  - Scroll to zoom
  - Zoom buttons for touch devices

### Visualization Options
- Toggle between 2D and 3D views
- Show/hide grid lines
- Elevation-based shading
- Export current frame as PNG image

## Usage

### Running Locally

1. Navigate to the web viewer directory:
   ```bash
   cd visualization/web
   ```

2. Start a local web server (Python 3):
   ```bash
   python -m http.server 8000
   ```
   
   Or with Node.js:
   ```bash
   npx http-server -p 8000
   ```

3. Open in browser:
   ```
   http://localhost:8000
   ```

### Loading Simulation Data

1. **From Scala Simulation**:
   - Export simulation using `JSONExporter`:
   ```scala
   JSONExporter.exportSimulation(
     states,
     Paths.get("output/simulation.json"),
     deltaCompression = true
   )
   ```

2. **In Web Viewer**:
   - Click "Load Simulation" button
   - Select the exported JSON file
   - Or drag & drop the file onto the page

### JSON Data Format

The viewer expects JSON data in the following format:

```json
{
  "metadata": {
    "width": 100,
    "height": 100,
    "timesteps": 1000,
    "deltaTime": 0.1,
    "parameters": {
      "sparkProbability": 0.001,
      "windSpeed": 5.0,
      "humidity": 0.3,
      "baseTemperature": 20.0
    },
    "terrain": {
      "minElevation": 400.0,
      "maxElevation": 2500.0,
      "hasSwissData": true
    }
  },
  "frames": [
    {
      "time": 0.0,
      "metrics": {
        "activeFires": 0,
        "burntArea": 0,
        "largestCluster": 0,
        "treeDensity": 0.7,
        "percolationIndicator": 0.0
      },
      "cells": [
        {
          "x": 50,
          "y": 50,
          "state": "Tree",
          "elevation": 1200.0,
          "vegetation": "Coniferous",
          "moisture": 0.4,
          "temperature": 20.0,
          "fireIntensity": 0.0
        }
      ],
      "fullFrame": true
    }
  ]
}
```

## Cell States

- **Empty**: Light gray - No vegetation
- **Tree**: Green (varies with moisture) - Living vegetation
- **Burning**: Yellow to red gradient based on intensity
- **Burnt**: Dark gray - Post-fire state

## Performance Considerations

- **Large Simulations**: The viewer uses delta compression to handle large simulations efficiently
- **Frame Skipping**: When jumping far in the timeline, frames are reconstructed as needed
- **Visible Area Rendering**: Only visible cells are rendered in 2D mode
- **WebGL Acceleration**: 3D view uses hardware acceleration

## Browser Requirements

- Modern browser with ES6 support
- WebGL support for 3D visualization
- Recommended: Chrome, Firefox, Safari, Edge (latest versions)

## Troubleshooting

### File Won't Load
- Ensure file is valid JSON format
- Check browser console for error messages
- Verify file size isn't too large (recommended < 100MB)

### Poor Performance
- Try disabling grid lines for large simulations
- Use 2D view for better performance
- Reduce playback speed for complex simulations

### 3D View Issues
- Ensure WebGL is enabled in browser
- Update graphics drivers
- Try different browser if issues persist

## Development

### Adding New Features

1. **New Cell States**: Update `getCellColor()` in `grid2d.js`
2. **New Metrics**: Add to metrics display in `index.html` and update `updateFrameUI()`
3. **New Controls**: Add UI elements and event handlers in `viewer.js`

### Building for Production

Currently, the viewer uses vanilla JavaScript with ES6 modules. For production deployment:

1. Consider bundling with webpack/rollup
2. Minify CSS and JavaScript
3. Add service worker for offline support
4. Implement progressive loading for very large files

## Credits

- Three.js for 3D visualization
- Forest fire simulation by EPFL Computational Physics Team