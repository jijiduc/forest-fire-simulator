# Forest Fire Simulator

Agent-based forest fire simulation using cellular automata. Scala implementation for studying wildfire dynamics, phase transitions, and climate change impacts on forest fire behavior in Alpine ecosystems.

## Academic Context
This is an academic project from both **205.1 - Functional Programming** and **204.2 - Computational Physics** courses, part of a Data Science Bsc at the Engineering School of Sion (HES-SO Valais-Wallis).

**Client:** Office Fédéral de l'Environnement  
**Objective:** Evaluate how climate change can influence the frequency and severity of forest fires.

The project is under the guidance of:
- Prof. Dr Pierre-André Mudry
- Prof. Dr Florian Desmons  
- Prof. Dr Jessen Page
- Prof. Dr Cédric Travelletti

## Project Status

### ✅ Completed Components

#### Core Models
- **Cell**: Grid cell with state (Empty, Tree, Burning, Burnt), position, elevation, vegetation type, moisture, and temperature
- **Grid**: 2D grid structure with functional operations (get, update, map, neighbors)
- **Terrain**: Elevation map with slope calculation, aspect determination, and vegetation assignment based on altitude
- **Climate**: Seasonal climate model with temperature, humidity, wind, and precipitation
- **Season**: Four seasons with distinct characteristics (temperature, humidity, precipitation probability, snow line)

#### Terrain Generation
- **TerrainGenerator**: Perlin noise-based realistic terrain generation with configurable parameters

#### Fire Dynamics (Phase 1.1 Complete)
- **FireDynamics**: Physically accurate fire spread model implementing:
  - Ignition probability model: `P_ignite = P_base × M(moisture) × T(temp) × V(vegetation) × S(slope) × W(wind) × O(oxygen)`
  - Empirical wind factor: `K_w = e^(0.1783 × V)`
  - Empirical slope factor: `K_φ = e^(3.533 × (tan(φ))^1.2)`
  - Heat transfer mechanisms (radiative, convective, pre-heating)
  - Moisture dynamics with evaporation and precipitation
  - Fire spread rate calculation
  - Fuel depletion model
  - Extinction probability calculation

#### Grid Initialization
- **GridInitializer**: Creates initial grid state based on terrain and climate conditions


## Installation
See [INSTALLATION.md](INSTALLATION.md) for detailed setup instructions.

## Running Tests

To run the test suite for this project, use the following commands:

**Run all tests:**
```bash
sbt test
```

**Run tests continuously (re-runs when files change):**
```bash
sbt ~test
```

**Run a specific test suite:**
```bash
sbt "testOnly models.CellSpec"
sbt "testOnly terrain.TerrainGeneratorSpec"
```

**Run tests with more detailed output:**
```bash
sbt "test -- -oDF"
```

**Run tests matching a pattern:**
```bash
sbt "testOnly *GridSpec"
```

**Generate test coverage report (requires plugin):**
```bash
sbt clean coverage test coverageReport
```

The test results will show in your terminal with green checkmarks for passed tests and red X's for failures.

## Author
- [Jeremy Duc](https://github.com/jijiduc)

