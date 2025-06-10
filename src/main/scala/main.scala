import models._
import terrain.TerrainGenerator
import simulation._
import cats.effect._
import cats.implicits._

object ForestFireSimulator extends IOApp.Simple {
  
  def run: IO[Unit] = {
    for {
      _ <- IO.println("Forest Fire Simulator - Alpine Ecosystem")
      _ <- IO.println("=========================================")
      
      width = 100
      height = 100
      
      _ <- IO.println(s"Generating ${width}x${height} alpine terrain...")
      terrain = TerrainGenerator.generateTerrain(width, height)
      
      climate = Climate(
        season = Season.Summer,
        wind = Wind(direction = math.Pi / 4, speed = 5.0),
        humidity = 0.4,
        precipitation = 0.0
      )
      
      _ <- IO.println(s"Initializing grid for ${climate.season} season...")
      grid = GridInitializer.initializeGrid(terrain, climate)
      
      // Create initial simulation state
      initialState = SimulationState(
        grid = grid,
        climate = climate,
        terrain = terrain,
        timeStep = 0.1,
        elapsedTime = 0.0,
        metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
        eventLog = List.empty
      )
      
      // Collect initial metrics
      metrics = MetricsCollector.collectMetrics(initialState)
      stateWithMetrics = initialState.withMetrics(metrics)
      
      // Calculate statistics
      stats = grid.cellsWithPosition.groupBy(_._3.state).map { case (state, cells) =>
        state -> cells.length
      }
      
      _ <- IO.println("\nInitial cell distribution:")
      _ <- stats.toList.traverse { case (state, count) =>
        IO.println(s"  $state: $count cells")
      }
      
      elevationStats = grid.cellsWithPosition.groupBy { case (_, _, cell) =>
        (cell.elevation / 500).toInt * 500
      }.map { case (elevation, cells) =>
        elevation -> cells.length
      }.toSeq.sortBy(_._1)
      
      _ <- IO.println("\nElevation distribution:")
      _ <- elevationStats.traverse { case (elevation, count) =>
        IO.println(f"  ${elevation}%4d - ${elevation + 500}%4d m: $count%5d cells")
      }
      
      _ <- IO.println("\nSimulation Metrics:")
      _ <- IO.println(f"  Tree density: ${metrics.treeDensity * 100}%.1f%%")
      _ <- IO.println(f"  Average moisture: ${metrics.averageMoisture * 100}%.1f%%")
      _ <- IO.println(f"  Percolation indicator: ${metrics.percolationIndicator}%.3f")
      
      _ <- IO.println("\nPhase 1.1 (Fire Dynamics) ✓")
      _ <- IO.println("Phase 1.2 (Simulation Engine) ✓")
      _ <- IO.println("\nSimulation engine ready for execution!")
      
      // Example: Run a few simulation steps
      _ <- IO.println("\nRunning 10 simulation steps...")
      engine = IOSimulationEngine()
      config = SimulationConfig(
        maxSteps = 10,
        adaptiveTimeStep = true,
        boundaryCondition = AbsorbingBoundary
      )
      
      finalState <- engine.run(stateWithMetrics, 10, config).compile.last
      
      _ <- finalState match {
        case Some(state) =>
          IO.println(f"\nAfter 10 steps (t=${state.elapsedTime}%.2f):")
            *> IO.println(f"  Active fires: ${state.metrics.activeFires}")
            *> IO.println(f"  Burnt area: ${state.metrics.totalBurntArea}")
            *> IO.println(f"  Percolation: ${state.metrics.percolationIndicator}%.3f")
        case None =>
          IO.println("No simulation states generated")
      }
      
    } yield ()
  }
}