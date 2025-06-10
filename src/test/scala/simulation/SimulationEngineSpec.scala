package simulation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.unsafe.implicits.global
import models._
import terrain.TerrainGenerator

class SimulationEngineSpec extends AnyFlatSpec with Matchers {
  
  def createTestState(): SimulationState = {
    val terrain = TerrainGenerator.generateTerrain(10, 10, 12345L)
    val climate = Climate(
      season = Season.Summer,
      wind = Wind(0.0, 5.0),
      humidity = 0.3,
      precipitation = 0.0
    )
    val grid = GridInitializer.initializeGrid(terrain, climate, 12345L)
    
    SimulationState(
      grid = grid,
      climate = climate,
      terrain = terrain,
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = MetricsCollector.collectMetrics(
        SimulationState(grid, climate, terrain, 0.1, 0.0, 
          SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0), List.empty)
      ),
      eventLog = List.empty
    )
  }
  
  "IOSimulationEngine" should "execute a single step" in {
    val engine = IOSimulationEngine()
    val initialState = createTestState()
    val config = SimulationConfig()
    
    val result = engine.step(initialState, config).unsafeRunSync()
    
    result.elapsedTime should be > initialState.elapsedTime
    result.timeStep should be > 0.0
  }
  
  it should "run for a fixed number of steps" in {
    val engine = IOSimulationEngine()
    val initialState = createTestState()
    val config = SimulationConfig(maxSteps = 10)
    
    val states = engine.run(initialState, 10, config).compile.toList.unsafeRunSync()
    
    states.length shouldBe 11 // Initial + 10 steps
    states.last.elapsedTime should be > 0.0
  }
  
  it should "maintain grid dimensions throughout simulation" in {
    val engine = IOSimulationEngine()
    val initialState = createTestState()
    val config = SimulationConfig()
    
    val states = engine.run(initialState, 5, config).compile.toList.unsafeRunSync()
    
    states.foreach { state =>
      state.grid.width shouldBe initialState.grid.width
      state.grid.height shouldBe initialState.grid.height
    }
  }
  
  it should "handle adaptive time stepping" in {
    val engine = IOSimulationEngine()
    val initialState = createTestState()
    val config = SimulationConfig(adaptiveTimeStep = true, maxTime = 1.0)
    
    val states = engine.runAdaptive(initialState, 1.0, config).compile.toList.unsafeRunSync()
    
    // Should have at least initial state and some steps
    states.size should be >= 2
    
    // Check that we don't exceed max time
    states.foreach { state =>
      state.elapsedTime should be <= 1.0
    }
    
    // Time steps might vary
    if (states.size > 2) {
      val timeSteps = states.sliding(2).collect {
        case Seq(s1, s2) if s2.elapsedTime > s1.elapsedTime => s2.elapsedTime - s1.elapsedTime
      }.toList
      
      timeSteps.size should be >= 1
    }
  }
  
  it should "stop when condition is met" in {
    val engine = IOSimulationEngine()
    val initialState = createTestState()
    val config = SimulationConfig()
    
    // Stop after 0.5 seconds
    val condition = (state: SimulationState) => state.elapsedTime >= 0.5
    
    val states = engine.runUntil(initialState, condition, config).compile.toList.unsafeRunSync()
    
    states.dropRight(1).foreach { state =>
      state.elapsedTime should be < 0.5
    }
  }
  
  it should "collect metrics at each step" in {
    val engine = IOSimulationEngine()
    val initialState = createTestState()
    val config = SimulationConfig()
    
    val states = engine.run(initialState, 3, config).compile.toList.unsafeRunSync()
    
    states.foreach { state =>
      state.metrics.treeDensity should be >= 0.0
      state.metrics.treeDensity should be <= 1.0
      state.metrics.averageMoisture should be >= 0.0
      state.metrics.averageMoisture should be <= 1.0
    }
  }
  
  it should "be deterministic with fixed seed" in {
    val engine = IOSimulationEngine()
    val initialState = createTestState()
    val config = SimulationConfig(randomSeed = Some(42L))
    
    val run1 = engine.run(initialState, 5, config).compile.toList.unsafeRunSync()
    val run2 = engine.run(initialState, 5, config).compile.toList.unsafeRunSync()
    
    run1.zip(run2).foreach { case (s1, s2) =>
      s1.elapsedTime shouldBe s2.elapsedTime
      s1.metrics shouldBe s2.metrics
    }
  }
}

class BoundaryHandlerSpec extends AnyFlatSpec with Matchers {
  
  def createTestGrid(): Grid = {
    val cells = Vector.tabulate(5, 5) { (y, x) =>
      Cell(
        position = Position(x, y),
        state = Tree,
        elevation = 1000.0,
        vegetationType = VegetationType.DenseForest,
        moisture = 0.5,
        temperature = 20.0
      )
    }
    Grid(cells, 5, 5)
  }
  
  "BoundaryHandler" should "handle periodic boundaries correctly" in {
    val grid = createTestGrid()
    
    // Corner cell should have 8 neighbors with periodic boundary
    val neighbors = BoundaryHandler.getNeighborsWithBoundary(grid, 0, 0, PeriodicBoundary)
    neighbors.length shouldBe 8
    
    // Should include wrapped cells
    neighbors.map(_.position) should contain (Position(4, 4)) // Diagonal wrap
    neighbors.map(_.position) should contain (Position(4, 0)) // Left wrap
    neighbors.map(_.position) should contain (Position(0, 4)) // Top wrap
  }
  
  it should "handle absorbing boundaries correctly" in {
    val grid = createTestGrid()
    
    // Corner cell should have only 3 neighbors with absorbing boundary
    val neighbors = BoundaryHandler.getNeighborsWithBoundary(grid, 0, 0, AbsorbingBoundary)
    neighbors.length shouldBe 3
  }
  
  it should "handle reflective boundaries correctly" in {
    val grid = createTestGrid()
    
    // Edge cell should see reflected neighbors
    val neighbors = BoundaryHandler.getNeighborsWithBoundary(grid, 0, 2, ReflectiveBoundary)
    // Reflective boundary removes duplicates, so we get fewer than 8 neighbors
    neighbors.length should be <= 8
    neighbors.length should be >= 3
    
    // All neighbors should be within grid bounds
    neighbors.foreach { neighbor =>
      neighbor.position.x should be >= 0
      neighbor.position.x should be < grid.width
      neighbor.position.y should be >= 0
      neighbor.position.y should be < grid.height
    }
  }
}

class MetricsCollectorSpec extends AnyFlatSpec with Matchers {
  
  def createGridWithFire(): Grid = {
    val cells = Vector.tabulate(10, 10) { (y, x) =>
      val state = if (x >= 4 && x <= 6 && y >= 4 && y <= 6) Burning else Tree
      Cell(
        position = Position(x, y),
        state = state,
        elevation = 1000.0,
        vegetationType = VegetationType.DenseForest,
        moisture = 0.5,
        temperature = if (state == Burning) 500.0 else 20.0
      )
    }
    Grid(cells, 10, 10)
  }
  
  "MetricsCollector" should "count active fires correctly" in {
    val grid = createGridWithFire()
    val state = SimulationState(
      grid, Climate(Season.Summer, Wind(0, 0), 0.5, 0.0),
      Terrain(Vector.fill(10)(Vector.fill(10)(1000.0)), 10, 10),
      0.1, 0.0, SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0), List.empty
    )
    
    val metrics = MetricsCollector.collectMetrics(state)
    metrics.activeFires shouldBe 9 // 3x3 burning area
  }
  
  it should "find largest fire cluster" in {
    val grid = createGridWithFire()
    val state = SimulationState(
      grid, Climate(Season.Summer, Wind(0, 0), 0.5, 0.0),
      Terrain(Vector.fill(10)(Vector.fill(10)(1000.0)), 10, 10),
      0.1, 0.0, SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0), List.empty
    )
    
    val metrics = MetricsCollector.collectMetrics(state)
    metrics.largestFireClusterSize shouldBe 9
  }
  
  it should "calculate tree density" in {
    val grid = createGridWithFire()
    val state = SimulationState(
      grid, Climate(Season.Summer, Wind(0, 0), 0.5, 0.0),
      Terrain(Vector.fill(10)(Vector.fill(10)(1000.0)), 10, 10),
      0.1, 0.0, SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0), List.empty
    )
    
    val metrics = MetricsCollector.collectMetrics(state)
    metrics.treeDensity shouldBe 0.91 +- 0.01 // 91 trees out of 100 cells
  }
}