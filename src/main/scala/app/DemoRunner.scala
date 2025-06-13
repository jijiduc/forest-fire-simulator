package app

import cats.effect._
import cats.effect.std.Console
import cats.effect.unsafe.implicits.global
import cats.implicits._
import cli._
import models._
import simulation._
import terrain.TerrainGenerator
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class DemoRunner[F[_]: Async: Console](
  config: AppConfig,
  progressReporter: ProgressReporter[F]
) {
  
  implicit def logger: Logger[F] = Slf4jLogger.getLogger[F]
  
  def run(command: DemoCommand): F[ExitCode] = {
    command.demoType match {
      case DemoTypes.QuickStart => runQuickStartDemo()
      case DemoTypes.WallisClimate => runWallisClimateDemo()
      case DemoTypes.PhaseTransition => runPhaseTransitionDemo()
      case "regions" => RegionScenarioDemo.run[F] *> Sync[F].pure(ExitCode.Success)
      case _ => Console[F].errorln(s"Unknown demo type: ${command.demoType}") *> Sync[F].pure(ExitCode.Error)
    }
  }.handleErrorWith { error =>
    for {
      _ <- logger.error(error)("Demo failed")
      _ <- Console[F].errorln(ConsoleFormatter.formatError(error))
    } yield ExitCode.Error
  }
  
  private def runQuickStartDemo(): F[ExitCode] = for {
    _ <- Console[F].println("\n🎮 Quick Start Demo")
    _ <- Console[F].println("=" * 60)
    _ <- Console[F].println("\nThis demo shows a basic forest fire simulation on synthetic terrain.")
    
    // Generate simple terrain
    _ <- Console[F].println("\n🏔️  Generating 50x50 alpine terrain...")
    terrain = TerrainGenerator.generateTerrain(50, 50)
    
    // Create climate
    climate = Climate(
      season = Season.Summer,
      wind = Wind(direction = Math.PI / 4, speed = 5.0),
      humidity = 0.4,
      precipitation = 0.0
    )
    
    _ <- Console[F].println(s"🌡️  Climate: ${climate.season}, ${climate.season.baseTemperature}°C, ${(climate.humidity * 100).toInt}% humidity")
    _ <- Console[F].println(s"💨 Wind: ${climate.wind.speed} m/s from ${formatDirection(climate.wind.direction)}")
    
    // Initialize grid
    grid = GridInitializer.initializeGrid(terrain, climate)
    
    // Show initial statistics
    stats = calculateGridStats(grid)
    _ <- Console[F].println(s"\n🌲 Initial forest:")
    _ <- Console[F].println(s"   Trees: ${stats.trees} (${(stats.treeDensity * 100).toInt}%)")
    _ <- Console[F].println(s"   Empty: ${stats.empty}")
    _ <- Console[F].println(s"   Water: ${stats.water}")
    
    // Run short simulation
    _ <- Console[F].println("\n🔥 Starting fire simulation (100 steps)...")
    
    initialState = SimulationState(
      grid = grid,
      climate = climate,
      terrain = terrain,
      timeStep = 0,
      elapsedTime = 0.0,
      metrics = MetricsCollector.collectMetrics(
        SimulationState(grid, climate, terrain, 0, 0.0, SimulationMetrics.empty, List.empty)
      ),
      eventLog = List.empty
    )
    
    // Use existing IOSimulationEngine with conversion
    ioEngine = new IOSimulationEngine()
    simulationConfig = SimulationConfig(
      maxSteps = 100,
      adaptiveTimeStep = true,
      boundaryCondition = AbsorbingBoundary
    )
    
    _ <- progressReporter.start(100, "Simulating")
    
    // Convert IO stream to F stream
    finalState <- Sync[F].delay {
      ioEngine.run(initialState, 100, simulationConfig)
        .compile
        .toList
        .unsafeRunSync()
        .lastOption
        .getOrElse(initialState)
    }
    
    _ <- progressReporter.finish("Simulation complete!")
    
    // Show results
    _ <- Console[F].println("\n📊 Results:")
    _ <- Console[F].println(ConsoleFormatter.formatMetrics(finalState.metrics))
    
    // Show grid visualization
    _ <- Console[F].println("\n🗺️  Final state visualization:")
    _ <- displayMiniGrid(finalState.grid)
    
    _ <- Console[F].println("\n✅ Demo complete! Use 'forest-fire-simulator run' for full simulations.")
    
  } yield ExitCode.Success
  
  private def runWallisClimateDemo(): F[ExitCode] = for {
    _ <- Console[F].println("\n🏔️  Wallis Climate Comparison Demo")
    _ <- Console[F].println("=" * 60)
    _ <- Console[F].println("\nThis demo compares fire behavior under different climate scenarios.")
    
    // Generate Wallis-like terrain
    terrain <- Sync[F].delay(TerrainGenerator.generateTerrain(30, 30))
    
    // Define climate scenarios
    scenarios = List(
      ("Current Climate (2020)", Climate(
        season = Season.Summer,
        wind = Wind(direction = 3 * Math.PI / 4, speed = 3.0), // NW wind (foehn)
        humidity = 0.5,
        precipitation = 0.0
      )),
      ("RCP 4.5 (2050)", Climate(
        season = Season.Summer,
        humidity = 0.45,     // -10% relative
        wind = Wind(direction = 3 * Math.PI / 4, speed = 3.5),
        precipitation = 0.0
      )),
      ("RCP 8.5 (2100)", Climate(
        season = Season.Summer,
        humidity = 0.4,      // -20% relative
        wind = Wind(direction = 3 * Math.PI / 4, speed = 4.0),
        precipitation = 0.0
      ))
    )
    
    results <- scenarios.traverse { case (name, climate) =>
      for {
        _ <- Console[F].println(s"\n🌡️  Scenario: $name")
        result <- runMiniSimulation(terrain, climate)
        _ <- Console[F].println(f"   Final burnt area: ${result.burntFraction * 100}%.1f%%")
        _ <- Console[F].println(f"   Max fire intensity: ${result.maxIntensity}%.1f")
        _ <- Console[F].println(f"   Fire duration: ${result.duration} steps")
      } yield (name, result)
    }
    
    // Summary comparison
    _ <- Console[F].println("\n📊 Climate Impact Summary:")
    _ <- Console[F].println(ConsoleFormatter.formatTable(
      headers = List("Scenario", "Burnt Area (%)", "Max Intensity", "Duration"),
      rows = results.map { case (name, r) =>
        List(name, f"${r.burntFraction * 100}%.1f", f"${r.maxIntensity}%.1f", r.duration.toString)
      }
    ))
    
    _ <- Console[F].println("\n✅ Demo complete! Climate change significantly increases fire severity.")
    
  } yield ExitCode.Success
  
  private def runPhaseTransitionDemo(): F[ExitCode] = for {
    _ <- Console[F].println("\n🔬 Phase Transition Demo")
    _ <- Console[F].println("=" * 60)
    _ <- Console[F].println("\nThis demo shows how fire behavior changes with tree density.")
    
    // Fixed terrain and climate
    terrain <- Sync[F].delay(TerrainGenerator.generateTerrain(30, 30, seed = 12345)) // Use fixed seed for flat-like terrain
    climate = Climate(
      season = Season.Summer,
      wind = Wind(direction = 0, speed = 2.0),
      humidity = 0.5,
      precipitation = 0.0
    )
    
    // Test different tree densities
    densities = List(0.3, 0.4, 0.5, 0.59, 0.65, 0.7, 0.8)
    
    _ <- Console[F].println("\n🌲 Testing tree densities from 30% to 80%...")
    
    results <- densities.traverse { density =>
      for {
        _ <- Console[F].print(f"   Density ${density * 100}%.0f%%: ")
        grid = createGridWithDensity(terrain, climate, density)
        result <- runMiniSimulation(terrain, climate, Some(grid))
        phase = if (result.percolated) "PERCOLATING" else "LOCALIZED"
        _ <- Console[F].println(f"burnt ${result.burntFraction * 100}%.1f%% - $phase")
      } yield (density, result)
    }
    
    // Find critical density
    criticalDensity = findCriticalDensity(results)
    
    _ <- Console[F].println(f"\n🎯 Critical tree density ≈ ${criticalDensity * 100}%.0f%%")
    _ <- Console[F].println("\nPhase Diagram:")
    _ <- displayPhaseDiagram(results)
    
    _ <- Console[F].println("\n✅ Demo complete! The system shows a clear percolation transition.")
    
  } yield ExitCode.Success
  
  // Helper methods
  private def formatDirection(radians: Double): String = {
    val degrees = (radians * 180 / Math.PI + 360) % 360
    val directions = Array("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    val index = ((degrees + 22.5) / 45).toInt % 8
    directions(index)
  }
  
  private def calculateGridStats(grid: Grid): GridStats = {
    val cells = grid.cellsWithPosition.map(_._3)
    GridStats(
      trees = cells.count(_.state == Tree),
      empty = cells.count(_.state == Empty),
      water = cells.count(_.vegetationType == VegetationType.Water),
      treeDensity = cells.count(_.state == Tree).toDouble / cells.length
    )
  }
  
  private def displayMiniGrid(grid: Grid): F[Unit] = {
    val visualization = (0 until Math.min(20, grid.height)).map { y =>
      (0 until Math.min(40, grid.width)).map { x =>
        grid.get(x, y).map(cell => cell.state match {
          case Empty => if (cell.vegetationType == VegetationType.Water) "💧" else "·"
          case Tree => "🌲"
          case Burning => "🔥"
          case Burnt => "⬛"
        }).getOrElse(" ")
      }.mkString("")
    }.mkString("\n")
    
    Console[F].println(visualization)
  }
  
  private def runMiniSimulation(
    terrain: Terrain,
    climate: Climate,
    gridOpt: Option[Grid] = None
  ): F[SimulationResult] = Sync[F].delay {
    val grid = gridOpt.getOrElse(GridInitializer.initializeGrid(terrain, climate))
    val initialState = SimulationState(
      grid = grid,
      climate = climate,
      terrain = terrain,
      timeStep = 0,
      elapsedTime = 0.0,
      metrics = MetricsCollector.collectMetrics(
        SimulationState(grid, climate, terrain, 0, 0.0, SimulationMetrics.empty, List.empty)
      ),
      eventLog = List.empty
    )
    
    // Run synchronous simulation for demo
    val engine = new SimulationEngine[IO] {
      def step(state: SimulationState, config: SimulationConfig): IO[SimulationState] = IO {
        // Update all cells
        val cells = state.grid.cellsWithPosition.map { case (x, y, cell) =>
          val neighbors = state.grid.neighbors(x, y)
          val (updatedCell, _) = CellUpdateLogic.updateCell(
            cell, neighbors, state.climate, state.terrain, 0.1
          )
          updatedCell
        }
        
        // Rebuild grid
        val cellsMatrix = cells.grouped(state.grid.width).map(_.toVector).toVector
        val updatedGrid = Grid(cellsMatrix, state.grid.width, state.grid.height)
        
        val newState = state.copy(
          grid = updatedGrid,
          timeStep = state.timeStep + 1,
          elapsedTime = state.elapsedTime + 0.1
        )
        newState.copy(metrics = MetricsCollector.collectMetrics(newState))
      }
      
      def run(initial: SimulationState, steps: Int, config: SimulationConfig): fs2.Stream[IO, SimulationState] = 
        fs2.Stream.empty
      
      def runUntil(initial: SimulationState, condition: SimulationState => Boolean, config: SimulationConfig): fs2.Stream[IO, SimulationState] = 
        fs2.Stream.empty
      
      def runAdaptive(initial: SimulationState, maxTime: Double, config: SimulationConfig): fs2.Stream[IO, SimulationState] = 
        fs2.Stream.empty
    }
    
    // Run for up to 200 steps or until fire dies out
    var state = initialState
    var maxIntensity = 0.0
    var duration = 0
    
    for (i <- 0 until 200) {
      state = engine.step(state, SimulationConfig()).unsafeRunSync()
      if (state.metrics.activeFires > 0) {
        duration = i + 1
        maxIntensity = Math.max(maxIntensity, state.metrics.activeFires.toDouble)
      }
    }
    
    SimulationResult(
      burntFraction = state.metrics.totalBurntArea.toDouble / (grid.width * grid.height),
      maxIntensity = maxIntensity,
      duration = duration,
      percolated = state.metrics.percolationIndicator > 0.5
    )
  }
  
  private def createGridWithDensity(
    terrain: Terrain,
    climate: Climate,
    density: Double
  ): Grid = {
    val grid = GridInitializer.initializeGrid(terrain, climate)
    
    // Adjust tree density
    val cells = grid.cellsWithPosition.map { case (x, y, cell) =>
      if (cell.state == Tree && Math.random() > density) {
        cell.copy(state = Empty)
      } else if (cell.state == Empty && Math.random() < density && cell.elevation > 0) {
        cell.copy(state = Tree, vegetationType = VegetationType.Grassland, moisture = climate.humidity)
      } else {
        cell
      }
    }
    
    // Convert the flat vector to Vector[Vector[Cell]]
    val cellsMatrix = cells.grouped(grid.width).map(_.toVector).toVector
    Grid(cellsMatrix, grid.width, grid.height)
  }
  
  private def findCriticalDensity(results: List[(Double, SimulationResult)]): Double = {
    val percolating = results.filter(_._2.percolated)
    val nonPercolating = results.filterNot(_._2.percolated)
    
    if (percolating.isEmpty) 1.0
    else if (nonPercolating.isEmpty) 0.0
    else (percolating.map(_._1).min + nonPercolating.map(_._1).max) / 2
  }
  
  private def displayPhaseDiagram(results: List[(Double, SimulationResult)]): F[Unit] = {
    val diagram = results.map { case (density, result) =>
      val bar = "█" * ((result.burntFraction * 20).toInt)
      val phase = if (result.percolated) "P" else "L"
      f"${density * 100}%.0f%% |$bar%-20s| $phase"
    }.mkString("\n")
    
    Console[F].println(diagram) *>
    Console[F].println("\n(L = Localized, P = Percolating)")
  }
}

// Helper case classes
case class GridStats(
  trees: Int,
  empty: Int,
  water: Int,
  treeDensity: Double
)

case class SimulationResult(
  burntFraction: Double,
  maxIntensity: Double,
  duration: Int,
  percolated: Boolean
)