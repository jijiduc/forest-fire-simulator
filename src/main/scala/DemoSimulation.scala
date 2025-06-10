import models._
import terrain.TerrainGenerator
import simulation._
import analysis._
import cats.effect._
import cats.implicits._

object DemoSimulation extends IOApp.Simple {
  
  def run: IO[Unit] = {
    for {
      _ <- IO.println("=== Forest Fire Simulator Demo ===")
      _ <- IO.println("Demonstrating a forest fire spreading across terrain")
      _ <- IO.println("")
      
      // Create a larger grid for better visualization
      terrain = TerrainGenerator.generateTerrain(100, 100, seed = 42)
      
      // Summer conditions with moderate wind
      climate = Climate(
        season = Season.Summer,
        wind = Wind(direction = 45, speed = 15.0), // NE wind
        humidity = 0.25, // Low humidity (dry conditions)
        precipitation = 0.0
      )
      
      // Initialize grid with trees
      grid = GridInitializer.initializeGrid(terrain, climate, seed = 42)
      
      // Start multiple fires
      fireStarts = List((20, 20), (80, 80), (50, 10))
      gridWithFires = fireStarts.foldLeft(grid) { case (g, (x, y)) =>
        g.get(x, y).fold(g) { cell =>
          if (cell.state == Tree) {
            g.updated(x, y, cell.copy(state = Burning))
          } else g
        }
      }
      
      initialState = SimulationState(
        grid = gridWithFires,
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
        maxSteps = 500,
        adaptiveTimeStep = true,
        boundaryCondition = PeriodicBoundary
      )
      
      _ <- IO.println(s"Initial state:")
      _ <- printGridState(initialState.grid)
      _ <- IO.println(s"\nRunning simulation...")
      
      // Run simulation and capture snapshots
      states <- engine.run(initialState, 500, config)
        .evalTap { state =>
          if (state.elapsedTime.toInt % 5 == 0 && state.elapsedTime.toInt > 0) {
            IO.println(s"\nTime: ${state.elapsedTime.toInt} hours") *>
            printGridState(state.grid) *>
            IO.println(s"Active fires: ${state.metrics.activeFires}, Burnt area: ${state.metrics.totalBurntArea}")
          } else IO.unit
        }
        .compile
        .toList
      
      finalState = states.last
      
      _ <- IO.println(s"\n=== Simulation Complete ===")
      _ <- IO.println(s"Total simulation time: ${finalState.elapsedTime} hours")
      _ <- IO.println(s"Total burnt area: ${finalState.metrics.totalBurntArea} cells")
      burntFraction = finalState.metrics.totalBurntArea.toDouble / (finalState.grid.width * finalState.grid.height)
      _ <- IO.println(f"Percentage of grid burnt: ${burntFraction * 100}%.1f%%")
      averageSpreadRate = if (finalState.elapsedTime > 0) finalState.metrics.totalBurntArea / finalState.elapsedTime else 0.0
      _ <- IO.println(f"Average spread rate: ${averageSpreadRate}%.2f cells/hour")
      
      // Analyze the results
      _ <- IO.println(s"\n=== Phase Analysis ===")
      runner = new SimulationRunner[IO]()
      analyzer = new PhaseTransitionAnalyzer[IO](runner)
      
      // Check if fire percolated
      percolated = checkPercolation(finalState.grid)
      _ <- IO.println(s"Fire percolated across grid: $percolated")
      
      // Analyze spatial patterns
      correlations = SpatialCorrelations.twoPointCorrelation(finalState, maxDistance = 20)
      correlationLength = SpatialCorrelations.correlationLength(correlations)
      _ <- IO.println(f"Correlation length of burnt patches: ${correlationLength}%.2f cells")
      
    } yield ()
  }
  
  def printGridState(grid: Grid): IO[Unit] = {
    // Create a simple ASCII visualization
    val visualization = (0 until grid.height by 2).map { y =>
      (0 until grid.width by 2).map { x =>
        val cell = grid(x, y)
        cell.state match {
          case Tree => "🌲"
          case Burning => "🔥"
          case Burnt => "⬛"
          case _ => "  "
        }
      }.mkString("")
    }.mkString("\n")
    
    IO.println(visualization)
  }
  
  def checkPercolation(grid: Grid): Boolean = {
    // Check if fire reached from one edge to opposite edge
    val burntCells = for {
      x <- 0 until grid.width
      y <- 0 until grid.height
      if grid(x, y).state == Burnt
    } yield (x, y)
    
    // Check horizontal percolation
    val leftEdgeBurnt = burntCells.exists(_._1 == 0)
    val rightEdgeBurnt = burntCells.exists(_._1 == grid.width - 1)
    val horizontalPercolation = leftEdgeBurnt && rightEdgeBurnt
    
    // Check vertical percolation  
    val topEdgeBurnt = burntCells.exists(_._2 == 0)
    val bottomEdgeBurnt = burntCells.exists(_._2 == grid.height - 1)
    val verticalPercolation = topEdgeBurnt && bottomEdgeBurnt
    
    horizontalPercolation || verticalPercolation
  }
}