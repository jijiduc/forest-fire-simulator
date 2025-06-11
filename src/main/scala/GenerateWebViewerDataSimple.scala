import cats.effect._
import cats.implicits._
import models._
import simulation._
import io.exporters._
import simulation.rules._
import java.nio.file.Paths
import scala.util.Random

/**
 * Simple web viewer data generator with fire spread
 */
object GenerateWebViewerDataSimple extends IOApp.Simple {
  
  def run: IO[Unit] = {
    val gridSize = 50
    val random = new Random(42)
    
    // Create simple grid with trees
    val cells = Vector.tabulate(gridSize, gridSize) { (y, x) =>
      val elevation = 1200.0
      val state = if (random.nextDouble() < 0.7) Tree else Empty
      
      Cell(
        position = Position(x, y),
        state = state,
        elevation = elevation,
        vegetationType = VegetationType.DenseForest,
        moisture = 0.25,
        temperature = 22.0
      )
    }
    
    val grid = Grid(cells, gridSize, gridSize)
    
    // Create climate
    val climate = Climate(
      season = Season.Summer,
      wind = Wind(direction = math.Pi / 4, speed = 8.0),
      humidity = 0.3,
      precipitation = 0.0
    )
    
    // Create terrain
    val elevationData = Vector.fill(gridSize, gridSize)(1200.0)
    val terrain = Terrain(elevationData, gridSize, gridSize)
    
    // Start fire in center
    val centerX = gridSize / 2
    val centerY = gridSize / 2
    val gridWithFire = grid.get(centerX, centerY).fold(grid) { cell =>
      if (cell.state == Tree) {
        grid.updated(centerX, centerY, cell.copy(state = Burning))
      } else grid
    }
    
    // Create initial state
    val initialState = SimulationState(
      grid = gridWithFire,
      climate = climate,
      terrain = terrain,
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(1, 0, 1, 0.0, 0.0, 0.7, 0.25),
      eventLog = List(IgnitionEvent(0.0, Position(centerX, centerY)))
    )
    
    // Create rule engine
    import IgnitionRules._
    import BurningRules._
    import ExtinctionRules._
    import RecoveryRules._
    
    val ruleEngine = new RuleEngine[IO](
      ignitionRules = List(
        SparkIgnition[IO](0.0001),
        NeighborIgnition[IO]()
      ),
      burningRules = List(
        IntensityEvolution[IO](),
        FuelConsumption[IO]()
      ),
      extinctionRules = List(
        FuelDepletion[IO](),
        MoistureSuppression[IO]()
      ),
      recoveryRules = List(
        NaturalRegrowth[IO](0.0001)
      ),
      interventionRules = List.empty
    )
    
    // Create engine and config
    val engine = new IOSimulationEngine(ruleEngine = Some(ruleEngine))
    val config = SimulationConfig(
      maxSteps = 150,
      adaptiveTimeStep = true,
      boundaryCondition = AbsorbingBoundary
    )
    
    for {
      _ <- IO.println("=== Generating Web Viewer Data ===")
      _ <- IO.println(s"Grid: ${gridSize}x${gridSize}, Tree density: 70%")
      _ <- IO.println(s"Fire started at: ($centerX, $centerY)")
      _ <- IO.println("Running simulation...")
      
      // Run simulation
      states <- engine.run(initialState, config.maxSteps, config).compile.toList
      
      _ <- IO.println(s"Generated ${states.length} states")
      
      // Export JSON
      _ <- JSONExporter.exportSimulation(
        states = states,
        outputPath = Paths.get("visualization/web/data/fire_simulation.json"),
        deltaCompression = true,
        frameInterval = 1
      )
      
      // Export CSV
      _ <- SimpleCSVExporter.exportSimulationResults(
        states,
        Paths.get("visualization/web/data/fire_timeseries.csv")
      )
      
      // Export final snapshot
      finalState = states.last
      _ <- JSONExporter.exportGridSnapshot(
        grid = finalState.grid,
        time = finalState.elapsedTime,
        metrics = finalState.metrics,
        outputPath = Paths.get("visualization/web/data/fire_final.json")
      )
      
      // Print results
      _ <- IO.println("\n=== Results ===")
      _ <- IO.println(f"Final time: ${finalState.elapsedTime}%.2f")
      _ <- IO.println(s"Burnt area: ${finalState.metrics.totalBurntArea} cells")
      _ <- IO.println(s"Active fires: ${finalState.metrics.activeFires}")
      _ <- IO.println(f"Percolation: ${finalState.metrics.percolationIndicator}%.3f")
      
      burntPercent = finalState.metrics.totalBurntArea * 100.0 / (gridSize * gridSize)
      _ <- IO.println(f"Percentage burnt: $burntPercent%.1f%%")
      
      _ <- IO.println("\n✓ Data exported to visualization/web/data/")
      _ <- IO.println("Load 'fire_simulation.json' in the web viewer")
      
    } yield ()
  }
}