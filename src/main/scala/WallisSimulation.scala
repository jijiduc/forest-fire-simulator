import models._
import models.{Terrain, Grid}
import terrain.TerrainGenerator
import simulation._
import analysis._
import io.swiss._
import io.importers._
import io.exporters._
import cats.effect._
import cats.implicits._
import org.http4s.ember.client.EmberClientBuilder
import java.nio.file.{Path, Paths}

/**
 * Wallis canton forest fire simulation with real Swiss geodata
 */
object WallisSimulation extends IOApp.Simple {
  
  def run: IO[Unit] = {
    // Use HTTP client resource
    EmberClientBuilder.default[IO].build.use { httpClient =>
      for {
        _ <- IO.println("=== WALLIS FOREST FIRE SIMULATION ===")
        _ <- IO.println("Using real Swiss geodata and climate projections")
        _ <- IO.println("")
        
        // Define simulation area - small region for testing
        // Center around Visp: 46.29°N, 7.88°E (LV95: ~2,635,000 E, 1,128,000 N)
        testRegion = CoordinateSystems.LV95BoundingBox(
          minEast = 2630000,
          minNorth = 1125000,
          maxEast = 2640000,  // 10km wide
          maxNorth = 1135000  // 10km high
        )
        
        _ <- IO.println(s"Simulation region: ${testRegion.width/1000}km x ${testRegion.height/1000}km")
        _ <- IO.println(s"Center: ${testRegion.center.toWGS84}")
        
        // Run simulation with mock data (real API calls would require authentication)
        _ <- runMockSimulation(testRegion)
        
      } yield ()
    }
  }
  
  def runMockSimulation(region: CoordinateSystems.LV95BoundingBox): IO[Unit] = {
    for {
      _ <- IO.println("\n--- Using mock data for demonstration ---")
      
      // Create mock terrain based on typical Wallis topography
      terrain = createMockWallisTerrain(region)
      
      // Create mock climate
      baseClimate = Climate(
        season = Season.Summer,
        wind = Wind(direction = 225, speed = 20.0), // SW wind (Foehn)
        humidity = 0.35, // Dry conditions
        precipitation = 0.0
      )
      
      // Initialize grid with vegetation patterns
      grid: Grid = createMockWallisGrid(terrain, baseClimate)
      
      // Create initial state with multiple ignition points
      ignitionPoints = List((25, 25), (75, 75), (50, 90)) // Strategic fire starts
      gridWithFires: Grid = ignitionPoints.foldLeft(grid) { case (g, (x, y)) =>
        g.get(x, y).fold(g) { cell =>
          if (cell.state == Tree) g.updated(x, y, cell.copy(state = Burning))
          else g
        }
      }
      
      initialState = SimulationState(
        grid = gridWithFires,
        climate = baseClimate,
        terrain = terrain,
        timeStep = 0.1,
        elapsedTime = 0.0,
        metrics = MetricsCollector.collectMetrics(SimulationState(
          grid = gridWithFires,
          climate = baseClimate,
          terrain = terrain,
          timeStep = 0.1,
          elapsedTime = 0.0,
          metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
          eventLog = List.empty
        )),
        eventLog = List.empty
      )
      
      _ <- IO.println("\n--- Running baseline simulation ---")
      baselineResults <- runScenario(initialState, "Baseline (current climate)")
      
      // Simulate with climate projections
      _ <- IO.println("\n--- Running climate scenarios ---")
      
      // RCP 2.6 (optimistic)
      rcp26Climate = baseClimate.copy(
        humidity = baseClimate.humidity * 0.9, // 10% drier
        wind = baseClimate.wind.copy(speed = baseClimate.wind.speed * 1.1) // 10% windier
      )
      rcp26State = initialState.copy(climate = rcp26Climate)
      rcp26Results <- runScenario(rcp26State, "RCP 2.6 (2050)")
      
      // RCP 8.5 (pessimistic)
      rcp85Climate = baseClimate.copy(
        humidity = baseClimate.humidity * 0.7, // 30% drier
        wind = baseClimate.wind.copy(speed = baseClimate.wind.speed * 1.3) // 30% windier
      )
      rcp85State = initialState.copy(climate = rcp85Climate)
      rcp85Results <- runScenario(rcp85State, "RCP 8.5 (2050)")
      
      // Export results
      outputDir = Paths.get("output", "wallis_simulation")
      _ <- exportResults(
        Map(
          "baseline" -> baselineResults,
          "rcp26" -> rcp26Results,
          "rcp85" -> rcp85Results
        ),
        outputDir
      )
      
      // Compare scenarios
      _ <- IO.println("\n=== SCENARIO COMPARISON ===")
      _ <- IO.println(f"${"Scenario"}%-20s ${"Burnt Area"}%-12s ${"Spread Time"}%-12s ${"Percolated"}%-10s")
      _ <- IO.println("-" * 54)
      
      scenarios = List(
        ("Baseline", baselineResults),
        ("RCP 2.6 (2050)", rcp26Results),
        ("RCP 8.5 (2050)", rcp85Results)
      )
      
      _ <- scenarios.traverse { case (name, results) =>
        val finalState = results.last
        val burntArea = finalState.metrics.totalBurntArea
        val totalCells = finalState.grid.width * finalState.grid.height
        val burntPercent = (burntArea.toDouble / totalCells) * 100
        val spreadTime = finalState.elapsedTime
        val percolated = checkPercolation(finalState.grid)
        
        IO.println(f"$name%-20s ${burntPercent}%.1f%%       ${spreadTime}%.1f hrs      $percolated%s")
      }
      
      _ <- IO.println(s"\nResults exported to: $outputDir")
      
    } yield ()
  }
  
  def createMockWallisTerrain(region: CoordinateSystems.LV95BoundingBox): Terrain = {
    // Create realistic elevation profile for Wallis
    // Valley floor ~600-800m, slopes up to 2000m+
    val width = 100
    val height = 100
    val baseElevation = 600.0
    
    val elevations = Array.tabulate(height, width) { (y, x) =>
      // Valley in the middle, mountains on sides
      val centerX = width / 2.0
      val distFromCenter = math.abs(x - centerX) / centerX
      
      // Add some noise
      val noise = math.sin(x * 0.1) * 50 + math.cos(y * 0.1) * 30
      
      baseElevation + distFromCenter * 1400 + noise
    }
    
    val vectorData = elevations.map(_.toVector).toVector
    Terrain(vectorData, width, height)
  }
  
  def createMockWallisGrid(terrain: Terrain, climate: Climate): Grid = {
    val cells = Vector.tabulate(terrain.height, terrain.width) { (y, x) =>
      val elevation = terrain.elevationAt(x, y)
      val position = Position(x, y)
      
      // Vegetation based on elevation
      val (vegType, cellState) = elevation match {
        case e if e < 800 => (VegetationType.DenseForest, Tree) // Valley forests
        case e if e < 1200 => (VegetationType.SparseForest, Tree) // Lower slopes
        case e if e < 1600 => (VegetationType.Shrubland, if (math.random < 0.3) Tree else Empty)
        case e if e < 2000 => (VegetationType.Grassland, Empty)
        case _ => (VegetationType.Barren, Empty) // Alpine zone
      }
      
      // Adjust moisture based on elevation and vegetation
      val baseMoisture = climate.humidity
      val moisture = baseMoisture * (1.0 - (elevation - 600) / 3000) * vegType.fuelLoad
      
      Cell(
        position = position,
        state = cellState,
        elevation = elevation,
        vegetationType = vegType,
        moisture = moisture,
        temperature = 20.0 - (elevation - 600) * 0.0065 // Temperature lapse rate
      )
    }
    
    Grid(cells, terrain.width, terrain.height)
  }
  
  def runScenario(initialState: SimulationState, scenarioName: String): IO[List[SimulationState]] = {
    for {
      _ <- IO.println(s"Running: $scenarioName")
      
      engine = IOSimulationEngine()
      config = SimulationConfig(
        maxSteps = 1000,
        maxTime = 100.0,
        adaptiveTimeStep = true,
        boundaryCondition = AbsorbingBoundary
      )
      
      states <- engine.run(initialState, 1000, config)
        .takeWhile(_.metrics.activeFires > 0)
        .compile
        .toList
        
      finalState = if (states.nonEmpty) states.last else initialState
      
      _ <- IO.println(f"  Simulation time: ${finalState.elapsedTime}%.1f hours")
      _ <- IO.println(f"  Burnt area: ${finalState.metrics.totalBurntArea} cells")
      _ <- IO.println(f"  Max cluster size: ${finalState.metrics.largestFireClusterSize}")
      
    } yield states
  }
  
  def checkPercolation(grid: Grid): Boolean = {
    val burntCells = for {
      x <- 0 until grid.width
      y <- 0 until grid.height
      if grid(x, y).state == Burnt
    } yield (x, y)
    
    // Check if fire reached from one edge to opposite edge
    val leftEdge = burntCells.exists(_._1 == 0)
    val rightEdge = burntCells.exists(_._1 == grid.width - 1)
    val topEdge = burntCells.exists(_._2 == 0)
    val bottomEdge = burntCells.exists(_._2 == grid.height - 1)
    
    (leftEdge && rightEdge) || (topEdge && bottomEdge)
  }
  
  def exportResults(
    scenarios: Map[String, List[SimulationState]],
    outputDir: Path
  ): IO[Unit] = {
    for {
      _ <- IO(java.nio.file.Files.createDirectories(outputDir))
      
      // Export time series for each scenario
      _ <- scenarios.toList.traverse { case (name, states) =>
        SimpleCSVExporter.exportSimulationResults(
          states,
          outputDir.resolve(s"${name}_timeseries.csv")
        )
      }
      
      // Export final grid states
      _ <- scenarios.toList.traverse { case (name, states) =>
        if (states.nonEmpty) {
          SimpleCSVExporter.exportGridSnapshot(
            states.last.grid,
            outputDir.resolve(s"${name}_final_grid.csv")
          )
        } else IO.unit
      }
      
      // Create comparison summary
      _ <- exportComparisonSummary(scenarios, outputDir.resolve("comparison_summary.csv"))
      
    } yield ()
  }
  
  def exportComparisonSummary(
    scenarios: Map[String, List[SimulationState]],
    outputPath: Path
  ): IO[Unit] = IO {
    val writer = new java.io.PrintWriter(outputPath.toFile)
    try {
      writer.println("scenario,total_burnt_area,burn_duration,max_cluster_size,percolated")
      
      scenarios.foreach { case (name, states) =>
        if (states.nonEmpty) {
          val finalState = states.last
          val metrics = finalState.metrics
          val percolated = checkPercolation(finalState.grid)
          
          writer.println(
            s"$name,${metrics.totalBurntArea},${finalState.elapsedTime}," +
            s"${metrics.largestFireClusterSize},$percolated"
          )
        }
      }
    } finally {
      writer.close()
    }
  }
}