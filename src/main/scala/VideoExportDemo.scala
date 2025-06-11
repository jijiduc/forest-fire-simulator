import cats.effect._
import cats.implicits._
import models._
import simulation._
import io.exporters._
import simulation.rules._
import java.nio.file.Paths
import scala.concurrent.duration._
import scala.util.Random

object VideoExportDemo extends IOApp {
  
  def createDemoGrid(width: Int, height: Int): Grid = {
    val random = new Random(42)
    
    // Create cells with varied elevation and vegetation
    val cells = Vector.tabulate(height, width) { (y, x) =>
      // Simple elevation model - higher in the center
      val distFromCenter = math.sqrt(math.pow(x - width/2, 2) + math.pow(y - height/2, 2))
      val maxDist = math.sqrt(math.pow(width/2, 2) + math.pow(height/2, 2))
      val elevation = 1000 + (1 - distFromCenter / maxDist) * 1000
      
      // Vegetation based on elevation
      val vegetationType = VegetationType.fromElevation(elevation)
      
      // Tree probability based on vegetation type
      val treeProbability = vegetationType match {
        case VegetationType.DenseForest => 0.8
        case VegetationType.SparseForest => 0.6
        case VegetationType.Shrubland => 0.4
        case VegetationType.Grassland => 0.2
        case _ => 0.0
      }
      
      val state = if (random.nextDouble() < treeProbability) Tree else Empty
      
      Cell(
        position = Position(x, y),
        state = state,
        elevation = elevation,
        vegetationType = vegetationType,
        moisture = 0.3 + random.nextDouble() * 0.4,
        temperature = 20.0 + random.nextDouble() * 5.0
      )
    }
    
    Grid(cells, width, height)
  }
  
  def createDemoClimate: Climate = Climate(
    season = Season.Summer,
    wind = Wind(direction = math.Pi / 4, speed = 5.0), // Northeast wind
    humidity = 0.4,
    precipitation = 0.0
  )
  
  def createDemoTerrain(width: Int, height: Int): Terrain = {
    // Simple terrain with some variation
    val elevationData = Vector.tabulate(height, width) { (y, x) =>
      val distFromCenter = math.sqrt(math.pow(x - width/2, 2) + math.pow(y - height/2, 2))
      val maxDist = math.sqrt(math.pow(width/2, 2) + math.pow(height/2, 2))
      1000.0 + (1 - distFromCenter / maxDist) * 1000.0
    }
    
    Terrain(elevationData, width, height)
  }
  
  def createDemoConfig: SimulationConfig = SimulationConfig(
    maxSteps = 500,
    maxTime = 50.0,
    adaptiveTimeStep = true,
    minTimeStep = 0.01,
    maxTimeStep = 0.1,
    boundaryCondition = AbsorbingBoundary,
    updateStrategy = SynchronousUpdate,
    parallelism = 4
  )
  
  def createRuleEngine[F[_]: Sync]: RuleEngine[F] = {
    // Import rule instances
    import IgnitionRules._
    import BurningRules._
    import ExtinctionRules._
    import RecoveryRules._
    
    val ignitionRules = List(
      SparkIgnition[F](0.001),
      NeighborIgnition[F]()
    )
    
    val burningRules = List(
      IntensityEvolution[F](),
      FuelConsumption[F]()
    )
    
    val extinctionRules = List(
      FuelDepletion[F](),
      MoistureSuppression[F]()
    )
    
    val recoveryRules = List(
      NaturalRegrowth[F](0.0001)
    )
    
    val interventionRules = List.empty[Rule[F]]
    
    new RuleEngine[F](
      ignitionRules,
      burningRules,
      extinctionRules,
      recoveryRules,
      interventionRules
    )
  }
  
  def run(args: List[String]): IO[ExitCode] = {
    
    val program = for {
      _ <- IO.println("=== Forest Fire Video Export Demo ===")
      
      // Setup paths
      outputDir = Paths.get("output/video_export")
      _ <- IO.println(s"Output directory: $outputDir")
      
      // Create components
      width = 100
      height = 100
      grid = createDemoGrid(width, height)
      climate = createDemoClimate
      terrain = createDemoTerrain(width, height)
      config = createDemoConfig
      ruleEngine = createRuleEngine[IO]
      engine = IOSimulationEngine.withRuleEngine(ruleEngine)
      exporter = new CSVVideoDataExporter[IO](outputDir)
      
      // Export elevation data (needed for 3D visualization)
      _ <- IO.println("Exporting elevation data...")
      _ <- exporter.exportElevationData(grid)
      
      // Initial state with some fires
      initialState = {
        var state = SimulationState(
          grid = grid,
          climate = climate,
          terrain = terrain,
          timeStep = config.minTimeStep,
          elapsedTime = 0.0,
          metrics = SimulationMetrics(
            activeFires = 0,
            totalBurntArea = 0,
            largestFireClusterSize = 0,
            averageFireIntensity = 0.0,
            percolationIndicator = 0.0,
            treeDensity = 0.0,
            averageMoisture = 0.0
          ),
          eventLog = List.empty
        )
        
        // Start fires at a few locations
        val fireStarts = List((50, 50), (30, 70), (70, 30))
        fireStarts.foreach { case (x, y) =>
          val newGrid = state.grid.updated(x, y, state.grid(x, y).copy(state = Burning))
          state = state.withGrid(newGrid)
        }
        
        // Update metrics for initial state
        state.withMetrics(MetricsCollector.collectMetrics(state))
      }
      
      // Run simulation with video export
      _ <- IO.println("Running simulation with frame export...")
      _ <- IO.println("Export interval: every 10 steps")
      
      states <- VideoExportEngine.runWithVideoExport(
        engine = engine,
        exporter = exporter,
        initial = initialState,
        steps = config.maxSteps,
        config = config,
        exportInterval = 10
      ).compile.toList
      
      _ <- IO.println(s"Simulation completed. Generated ${states.length} states")
      
      // Export metadata
      allMetrics = states.map(_.metrics)
      _ <- exporter.exportMetadata(config, allMetrics)
      
      // Final statistics
      finalState = states.last
      burntCells = finalState.grid.cellsWithPosition.count(_._3.state == Burnt)
      totalCells = finalState.grid.width * finalState.grid.height
      burntPercentage = (burntCells.toDouble / totalCells) * 100
      
      _ <- IO.println(f"\nFinal Statistics:")
      _ <- IO.println(f"  Burnt cells: $burntCells / $totalCells (${burntPercentage}%.1f%%)")
      _ <- IO.println(f"  Final time: ${finalState.elapsedTime}%.1f")
      _ <- IO.println(f"  Percolation indicator: ${finalState.metrics.percolationIndicator}%.3f")
      
      _ <- IO.println("\n✓ Frame export completed!")
      _ <- IO.println("\nNext steps to generate video:")
      _ <- IO.println("1. Generate 2D frames:")
      _ <- IO.println(s"   python visualization/video/generate_frames.py $outputDir output/video_frames/2d_frames")
      _ <- IO.println("\n2. Generate 3D frames:")
      _ <- IO.println(s"   python visualization/video/terrain_3d.py $outputDir output/video_frames/3d_frames")
      _ <- IO.println("\n3. Compile final video:")
      _ <- IO.println("   python visualization/video/compile_video.py output/video_frames output/demo_video.mp4")
      
    } yield ExitCode.Success
    
    program.handleErrorWith { error =>
      IO.println(s"Error: ${error.getMessage}") *> 
      IO(error.printStackTrace()) *>
      IO.pure(ExitCode.Error)
    }
  }
}