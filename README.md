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

#### Simulation Engine (Phase 1.2 Complete)
- **IOSimulationEngine**: Pure functional simulation engine using cats-effect IO
- **SimulationState**: Immutable state with integrated metrics
- **UpdateStrategies**: Synchronous, asynchronous, and block synchronous cell updates
- **BoundaryHandler**: Periodic, reflective, absorbing, and fixed boundary conditions
- **MetricsCollector**: Fire metrics and percolation analysis:
  - Cluster detection algorithms
  - Percolation indicator (0-1 scale)
  - Fire size distribution analysis
  - System-spanning fire detection
- **SimulationConfig**: Adaptive time stepping with CFL condition

#### Rule System (Phase 1.3 Complete)
- **Modular Rule Architecture**: Extensible system for cell state transitions
- **Rule Categories**:
  - **IgnitionRules**: SparkIgnition, NeighborIgnition, EmberIgnition
  - **BurningRules**: IntensityEvolution, FuelConsumption, HeatGeneration, PreHeating
  - **ExtinctionRules**: FuelDepletion, MoistureSuppression, TemperatureDecay, NeighborIsolation
  - **RecoveryRules**: NaturalRegrowth, SeasonalGrowth, SeedDispersion, VegetationSuccession
- **RuleEngine**: Orchestrates rule application based on cell states
- **RuleConfig**: Enable/disable specific rules and set parameters

#### Phase Transition Analysis (Phase 2.1 Complete)
- **PhaseParameter**: Control parameters (tree density, moisture, wind, temperature, spark probability)
- **OrderParameters**: System observables (burnt fraction, cluster statistics, percolation indicator)
- **PhaseIdentifier**: Automatic phase classification (sub-critical, critical, super-critical)
- **CriticalPointFinder**: Multiple algorithms for locating phase transitions:
  - Bisection search
  - Susceptibility peak method
  - Binder cumulant crossing
  - Critical exponent estimation
- **SimulationRunner**: Batch execution with ensemble averaging
- **PhaseTransitionAnalyzer**: Complete phase analysis orchestration

#### Statistical Mechanics (Phase 2.2 Complete)
- **StatisticalMechanics**: Finite-size scaling analysis
  - Critical exponent extraction (β, γ, ν, τ, α, δ, η)
  - Data collapse verification
  - Scaling function determination
- **CorrelationAnalysis**: Spatial and temporal correlations
  - Two-point correlation functions
  - Correlation length extraction
  - Directional analysis (anisotropy detection)
  - Critical slowing down
- **ResponseFunctions**: System response calculations
  - Magnetic susceptibility
  - Binder cumulant
  - Specific heat
  - Connected susceptibility
- **UniversalityClass**: Classification framework
  - 2D Isotropic Percolation (verified)
  - Directed Percolation
  - Self-Organized Criticality
  - Hyperscaling relation verification
- **MomentAnalysis**: Cluster statistics
  - Power-law fitting with cutoff
  - Fractal dimension extraction
  - Universal amplitude ratios
- **AdvancedStatistics**: Statistical methods
  - Bootstrap and jackknife resampling
  - Maximum likelihood estimation
  - Kolmogorov-Smirnov tests
  - Cross-validation
- **DataQuality**: Quality assessment
  - Finite-size effect detection
  - Equilibration verification
  - Statistical significance testing

#### Swiss Data Integration (Phase 3 Complete)
- **Coordinate Systems**:
  - LV95 (Swiss coordinate system)
  - WGS84 (GPS standard)
  - Bidirectional transformations (±200m precision)
  - Bounding box operations
- **Swiss Data Models**:
  - Land cover types (SwissLandStats mapping)
  - Fire danger levels (5 levels with multipliers)
  - Climate scenarios (RCP 2.6, 4.5, 8.5)
  - Canton boundaries (Wallis implemented)
- **Map Service Clients**:
  - WMS (Web Map Service) client
  - WMTS (Web Map Tile Service) client
  - STAC (SpatioTemporal Asset Catalog) client
- **Data Importers**:
  - Elevation data (25m, 50m, 100m resolution)
  - Land cover/vegetation mapping
  - Climate projections (CH2018)
  - Regional parameters (Foehn winds, altitude effects)
- **Wallis Simulation**:
  - 10km × 10km region centered on Rhône valley
  - Foehn wind effects (SE direction, ×1.5 speed, -30% humidity)
  - Altitude gradients (-0.65°C/100m)
  - Climate scenario comparisons
- **Data Exporters**:
  - CSV format for time series and phase data
  - Grid snapshots
  - Climate scenario parameters

#### Grid Initialization
- **GridInitializer**: Creates initial grid state based on terrain and climate conditions

### 📊 Test Coverage
- **Total Tests**: 181 unit tests (all passing)
- **Phase 1**: 83 tests covering core simulation
- **Phase 2**: 72 tests covering analysis framework
- **Phase 3**: 26 tests covering data integration


## Installation
See [INSTALLATION.md](INSTALLATION.md) for detailed setup instructions.

## Running the Simulation

### Basic Simulation
```bash
# Run with default parameters
sbt run

# Run with custom configuration
sbt "run --grid-size 100 --tree-density 0.6 --steps 1000"
```

### Wallis Canton Simulation
```bash
# Run the Wallis-specific simulation with real terrain data
sbt "runMain demos.WallisSimulation"
```

### Phase Analysis
```bash
# Run phase transition analysis
sbt "runMain analysis.PhaseTransitionDemo"
```

## Configuration

The simulation can be configured through various parameters:

- **Grid Size**: Width and height of the simulation grid
- **Tree Density**: Initial fraction of cells containing trees (0.0 to 1.0)
- **Climate Parameters**: Temperature, humidity, wind speed and direction
- **Simulation Steps**: Number of time steps to simulate
- **Boundary Conditions**: Periodic, reflective, absorbing, or fixed
- **Update Strategy**: Synchronous, asynchronous, or block synchronous

## Output

Simulation results are exported to the `output/` directory:
- `simulation_results.csv`: Time series of fire metrics
- `phase_data.csv`: Phase transition analysis results
- `grid_snapshots/`: Grid states at various time points
- `climate_scenarios.csv`: Climate parameter configurations

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

## Project Structure

```
forest-fire-simulator/
├── src/
│   ├── main/scala/
│   │   ├── models/           # Core domain models
│   │   ├── simulation/       # Simulation engine and rules
│   │   ├── analysis/         # Phase transition and statistical analysis
│   │   ├── terrain/          # Terrain generation
│   │   ├── io/              # Data import/export
│   │   │   ├── swiss/       # Swiss-specific data models
│   │   │   ├── importers/   # Data importers
│   │   │   └── exporters/   # Data exporters
│   │   └── demos/           # Demonstration applications
│   └── test/scala/          # Comprehensive test suite
├── docs/                    # Documentation
├── output/                  # Simulation results
├── build.sbt               # Build configuration
├── README.md               # This file
├── INSTALLATION.md         # Setup instructions
├── plan.md                 # Detailed implementation plan
└── report.tex/pdf          # Academic report

```

## Next Steps

### Immediate Priorities (Week 3)
- [ ] Climate scenario systematic analysis using imported Swiss data
- [ ] Advanced visualization pipeline (Python/ParaView integration)
- [ ] Performance optimization for large-scale simulations

### Future Enhancements (Week 4-5)
- [ ] Multi-scale modeling (crown vs ground fire)
- [ ] Machine learning integration for pattern recognition
- [ ] Real-time web dashboard
- [ ] Extended climate projections (2100+)

### Final Phase (Week 6)
- [ ] 15-second demo video (1080p60)
- [ ] Interactive presentation materials
- [ ] Final performance benchmarks
- [ ] Complete documentation

## Scientific Significance

This simulator provides insights into:
- **Percolation Theory**: Forest fires as a percolation problem on 2D lattices
- **Phase Transitions**: Critical tree density for system-spanning fires
- **Climate Impact**: Quantitative assessment of climate change on fire dynamics
- **Universal Behavior**: Identification of universality class (2D percolation)
- **Alpine Specifics**: Foehn winds and altitude effects on fire spread

## Author
- [Jeremy Duc](https://github.com/jijiduc)

## LLM Contribution
- Part of this program have been implemented with the help of Claude Opus 4 from Anthropic.

