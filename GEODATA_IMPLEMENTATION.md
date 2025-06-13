# Real Geodata Implementation

## Overview
This implementation adds support for using real Swiss geodata in the forest fire simulator, with multiple predefined region scenarios and configurable scale factors for testing different grid sizes.

## Key Features

### 1. Region Scenarios
Five predefined Swiss regions with realistic parameters:
- **finges**: Finges Forest (10×6 km) - Primary study area, high fire risk
- **visp-stalden**: Visp-Stalden Triangle (15×10 km) - Steep terrain study
- **leukerbad**: Leukerbad Valley (8×8 km) - Isolated system
- **sion-saviese**: Sion-Savièse Slopes (12×8 km) - Urban interface
- **test**: Small test area (2×2 km) - Quick experiments

### 2. Scale Factors
Four scale options to control grid resolution:
- **full**: 25m cells (maximum detail)
- **half**: 50m cells (balanced)
- **quarter**: 100m cells (faster)
- **tenth**: 250m cells (very fast)

### 3. Data Sources
- **Elevation**: Swiss DHM25/50/100 via WMS
- **Land Cover**: Swiss TLM via WMS
- **Climate**: MeteoSwiss projections (simulated)

## Usage Examples

### Basic Commands

```bash
# Synthetic terrain (fast, no internet required)
sbt "run run --region finges --scale quarter"

# Real geodata (requires internet)
sbt "run run --region finges --scale quarter --real-data"

# Different scales
sbt "run run --region test --scale full"       # 80×80 cells
sbt "run run --region test --scale half"       # 40×40 cells
sbt "run run --region test --scale quarter"    # 20×20 cells

# Climate scenarios
sbt "run run --region finges --scenario rcp45 --year 2050"
sbt "run run --region finges --scenario rcp85 --year 2100 --real-data"
```

### View Available Regions

```bash
# Show all region scenarios with grid sizes
sbt "run demo --type regions"
```

## Grid Sizes by Region and Scale

| Region | Area | Full (25m) | Half (50m) | Quarter (100m) | Tenth (250m) |
|--------|------|------------|------------|----------------|--------------|
| test | 4 km² | 80×80 | 40×40 | 20×20 | 8×8 |
| finges | 60 km² | 400×240 | 200×120 | 100×60 | 40×24 |
| leukerbad | 64 km² | 320×320 | 160×160 | 80×80 | 32×32 |
| visp-stalden | 150 km² | 600×400 | 300×200 | 150×100 | 60×40 |
| sion-saviese | 96 km² | 480×320 | 240×160 | 120×80 | 48×32 |

## Implementation Details

### GeoDataClient
- Fetches elevation and land cover data from Swiss WMS services
- Implements caching to avoid repeated downloads
- Converts image data to simulation grid format

### RegionScenarios
- Defines region boundaries in LV95 coordinates
- Stores physical characteristics (vegetation, slope, fire history)
- Calculates grid dimensions based on scale factor

### SwissDataImporter
- Enhanced to support real data fetching
- Falls back to synthetic data when offline
- Applies regional characteristics (Foehn winds, etc.)

### CLI Updates
- New `--scale` option for grid resolution
- New `--real-data` flag to enable geodata fetching
- Support for region scenario IDs

## Performance Considerations

| Scale | Typical Grid Size | Memory Usage | Simulation Speed |
|-------|------------------|--------------|------------------|
| full | 400×240 | ~300 MB | Slow |
| half | 200×120 | ~75 MB | Good |
| quarter | 100×60 | ~20 MB | Fast |
| tenth | 40×24 | ~3 MB | Very Fast |

## Caching
Downloaded geodata is cached in `cache/geodata/` to avoid repeated downloads:
- Cache key includes bounds and resolution
- GeoTIFF files for elevation
- PNG files for land cover

## Future Enhancements
1. Add more regions (Grisons, Ticino)
2. Implement real-time weather data integration
3. Add historical fire perimeter validation
4. Support for custom region definitions
5. Parallel tile downloading for large areas