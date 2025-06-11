import cats.effect._
import cats.implicits._
import models._
import simulation._
import io.exporters._
import simulation.rules._
import java.nio.file.Paths
import scala.util.Random

object VideoExportTest extends IOApp {
  
  def createSmallGrid(width: Int, height: Int): Grid = {
    val cells = Vector.tabulate(height, width) { (y, x) =>
      val state = if (x > 20 && x < 80 && y > 20 && y < 80 && Random.nextDouble() < 0.7) Tree else Empty
      
      Cell(
        position = Position(x, y),
        state = state,
        elevation = 1000.0 + Random.nextDouble() * 500,
        vegetationType = VegetationType.DenseForest,
        moisture = 0.3,
        temperature = 20.0
      )
    }
    
    Grid(cells, width, height)
  }
  
  def run(args: List[String]): IO[ExitCode] = {
    val program = for {
      _ <- IO.println("=== Video Export Test ===")
      
      // Setup
      outputDir = Paths.get("output/video_test")
      width = 100
      height = 100
      grid = createSmallGrid(width, height)
      climate = Climate(Season.Summer, Wind(0, 2), 0.4, 0.0)
      terrain = Terrain(Vector.fill(height, width)(1200.0), width, height)
      config = SimulationConfig(maxSteps = 50, maxTime = 5.0)
      
      // Simple rule engine with just neighbor ignition
      ruleEngine = {
        import IgnitionRules._
        import ExtinctionRules._
        new RuleEngine[IO](
          List(NeighborIgnition[IO]()),
          List.empty,
          List(FuelDepletion[IO]()),
          List.empty,
          List.empty
        )
      }
      
      engine = IOSimulationEngine.withRuleEngine(ruleEngine)
      exporter = new CSVVideoDataExporter[IO](outputDir)
      
      // Export elevation
      _ <- exporter.exportElevationData(grid)
      
      // Initial state with central fire
      initialState = {
        val state = SimulationState(
          grid = grid,
          climate = climate,
          terrain = terrain,
          timeStep = 0.1,
          elapsedTime = 0.0,
          metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
          eventLog = List.empty
        )
        
        // Start fire in center
        val newGrid = state.grid.updated(50, 50, state.grid(50, 50).copy(state = Burning))
        state.withGrid(newGrid).withMetrics(MetricsCollector.collectMetrics(state.withGrid(newGrid)))
      }
      
      // Run with export every 5 steps
      _ <- IO.println("Running simulation...")
      states <- VideoExportEngine.runWithVideoExport(
        engine, exporter, initialState, 50, config, exportInterval = 5
      ).compile.toList
      
      _ <- IO.println(s"Generated ${states.length} states")
      _ <- exporter.exportMetadata(config, states.map(_.metrics))
      
      // Count frames
      frameCount <- IO {
        val framesDir = outputDir.resolve("frames")
        if (framesDir.toFile.exists()) {
          framesDir.toFile.listFiles().count(_.getName.startsWith("frame_"))
        } else 0
      }
      
      _ <- IO.println(s"Exported $frameCount frames")
      _ <- IO.println(s"Output directory: $outputDir")
      
    } yield ExitCode.Success
    
    program.handleErrorWith { error =>
      IO.println(s"Error: ${error.getMessage}") *> 
      IO(error.printStackTrace()) *>
      IO.pure(ExitCode.Error)
    }
  }
}