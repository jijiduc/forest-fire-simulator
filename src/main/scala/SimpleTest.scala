import models._
import terrain.TerrainGenerator
import simulation._
import analysis._
import cats.effect._
import cats.implicits._

object SimpleTest extends IOApp.Simple {
  
  def run: IO[Unit] = {
    for {
      _ <- IO.println("=== Forest Fire Simulator Test ===")
      _ <- IO.println("Testing Phase 1 and Phase 2 Implementation")
      _ <- IO.println("")
      
      // Test Phase 1: Basic simulation
      _ <- testBasicSimulation()
      
      // Test Phase 2.1: Phase transitions
      _ <- testPhaseTransitions()
      
      // Test Phase 2.2: Statistical mechanics
      _ <- testStatisticalMechanics()
      
      _ <- IO.println("\n=== All tests completed successfully! ===")
    } yield ()
  }
  
  def testBasicSimulation(): IO[Unit] = {
    for {
      _ <- IO.println("--- Phase 1: Basic Simulation ---")
      
      // Create small test grid
      terrain = TerrainGenerator.generateTerrain(50, 50, seed = 42)
      climate = Climate(
        season = Season.Summer,
        wind = Wind(direction = 0, speed = 10.0),
        humidity = 0.3,
        precipitation = 0.0
      )
      
      grid = GridInitializer.initializeGrid(terrain, climate)
      
      // Set a fire
      centerCell = grid.get(25, 25).get
      burningCell = centerCell.copy(state = Burning)
      gridWithFire = grid.updated(25, 25, burningCell)
      
      initialState = SimulationState(
        grid = gridWithFire,
        climate = climate,
        terrain = terrain,
        timeStep = 0.1,
        elapsedTime = 0.0,
        metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
        eventLog = List.empty
      )
      
      // Run simulation
      engine = IOSimulationEngine()
      config = SimulationConfig(
        maxSteps = 100,
        adaptiveTimeStep = true,
        boundaryCondition = PeriodicBoundary
      )
      
      states <- engine.run(initialState, 100, config).compile.toList
      finalState = states.last
      
      _ <- IO.println(s"Simulation ran for ${states.length} steps")
      _ <- IO.println(s"Final time: ${finalState.elapsedTime}")
      _ <- IO.println(s"Burnt cells: ${finalState.metrics.totalBurntArea}")
      _ <- IO.println(s"Active fires: ${finalState.metrics.activeFires}")
      
    } yield ()
  }
  
  def testPhaseTransitions(): IO[Unit] = {
    for {
      _ <- IO.println("\n--- Phase 2.1: Phase Transitions ---")
      
      // Test critical point finding
      runner = new SimulationRunner[IO]()
      analyzer = new PhaseTransitionAnalyzer[IO](runner)
      
      climate = Climate(
        season = Season.Summer,
        wind = Wind(0, 5.0),
        humidity = 0.5,
        precipitation = 0.0
      )
      
      // Quick phase transition scan
      _ <- IO.println("Scanning tree density parameter...")
      result <- analyzer.analyzeTreeDensityTransition(
        climate,
        densityRange = ParameterRange(0.3, 0.7, 5)
      )
      
      _ <- IO.println(s"Found ${result.phaseData.length} parameter points")
      _ <- IO.println(f"Critical density: ${result.criticalDensity}%.3f (confidence: ${result.confidence}%.2f)")
      _ <- IO.println(f"Percolation threshold: ${result.percolationThreshold}%.3f")
      
      // Identify phases
      _ <- result.phaseData.traverse { case (density, phase, _) =>
        IO.println(f"  Tree density ${density}%.2f -> $phase")
      }
      
    } yield ()
  }
  
  def testStatisticalMechanics(): IO[Unit] = {
    for {
      _ <- IO.println("\n--- Phase 2.2: Statistical Mechanics ---")
      
      // Create test data
      terrain = TerrainGenerator.generateTerrain(40, 40)
      climate = Climate(Season.Summer, Wind(0, 5.0), 0.5, 0.0)
      grid = GridInitializer.initializeGrid(terrain, climate)
      
      state = SimulationState(
        grid = grid,
        climate = climate,
        terrain = terrain,
        timeStep = 0.1,
        elapsedTime = 0.0,
        metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
        eventLog = List.empty
      )
      
      // Test correlation analysis
      _ <- IO.println("Testing correlation analysis...")
      correlations = SpatialCorrelations.twoPointCorrelation(state, maxDistance = 10)
      correlationLength = SpatialCorrelations.correlationLength(correlations)
      _ <- IO.println(f"Correlation length: ${correlationLength}%.2f")
      
      // Test response functions
      _ <- IO.println("\nTesting response functions...")
      ensemble = EnsembleResults(
        results = List.fill(10)(
          SimulationResult(
            finalState = state,
            timeSeries = List.empty,
            orderParameters = Map("burntFraction" -> util.Random.nextDouble()),
            phase = Critical,
            runTime = 1.0
          )
        ),
        averageOrderParameters = Map("burntFraction" -> 0.5),
        standardDeviations = Map("burntFraction" -> 0.1),
        phase = Critical
      )
      
      susceptibility = ResponseFunctions.magneticSusceptibility(ensemble)
      binder = ResponseFunctions.binderCumulant(ensemble)
      
      _ <- IO.println(f"Susceptibility: ${susceptibility}%.3f")
      _ <- IO.println(f"Binder cumulant: ${binder}%.3f")
      
      // Test universality class
      _ <- IO.println("\nTesting universality classification...")
      exponents = CriticalExponents(
        beta = 0.14,
        gamma = 2.4,
        nu = 1.33
      )
      
      universalityClass = UniversalityClassifier.classifySystem(exponents, confidence = 0.8)
      _ <- IO.println(s"Identified universality class: ${universalityClass.name}")
      
      // Test data quality
      _ <- IO.println("\nTesting data quality assessment...")
      quality = DataQualityAnalysis.assessDataQuality(
        Map(40 -> List.empty),
        List(state),
        StatisticalMechanicsResults(
          criticalExponents = exponents,
          exponentErrors = Map("beta" -> 0.01, "gamma" -> 0.02, "nu" -> 0.01),
          universalityClass = universalityClass,
          scalingQuality = 0.95,
          correlationLength = correlationLength,
          dynamicExponent = None,
          hyperscalingViolation = 0.02
        )
      )
      
      _ <- IO.println(s"Data quality: ${quality.overallQuality}")
      
    } yield ()
  }
}