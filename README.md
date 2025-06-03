# forest-fire-simulator
Agent-based forest fire simulation using cellular automata. Scala implementation for studying wildfire dynamics, phase transitions.

## Academic Context
This is an academic project from both **205.1 - Functional Programming** and **204.2 - Computational Physics** courses, part of a Data Science Bsc at the Engineering School of Sion (HES-SO Valais-Wallis).

The project is under the guidance of:
- Prof. Dr Pierre-André Mudry
- Prof. Dr Florian Desmons  
- Prof. Dr Jessen Page
- Prof. Dr Cédric Travelletti

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

