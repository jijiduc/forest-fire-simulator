package analysis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import models._
import simulation._

class OrderParametersSpec extends AnyFlatSpec with Matchers {
  
  def createGrid(width: Int, height: Int, pattern: (Int, Int) => CellState): Grid = {
    val cells = Vector.tabulate(height, width) { (y, x) =>
      Cell(
        position = Position(x, y),
        state = pattern(x, y),
        elevation = 1500.0,
        vegetationType = VegetationType.DenseForest,
        moisture = 0.5,
        temperature = 20.0
      )
    }
    Grid(cells, width, height)
  }
  
  def createTestState(grid: Grid): SimulationState = {
    SimulationState(
      grid = grid,
      climate = Climate(Season.Summer, Wind(0, 5.0), 0.5, 0.0),
      terrain = Terrain(Vector.fill(grid.height)(Vector.fill(grid.width)(1500.0)), grid.width, grid.height),
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
  }
  
  "burntFraction" should "calculate correct fraction of burnt cells" in {
    val grid = createGrid(10, 10, (x, y) =>
      if (x < 3 && y < 3) Burnt else Tree
    )
    val state = createTestState(grid)
    
    val fraction = OrderParameters.burntFraction(state)
    fraction shouldBe 0.09 +- 0.01 // 9 burnt out of 100 cells
  }
  
  it should "exclude water cells from calculation" in {
    val cells = Vector.tabulate(10, 10) { (y, x) =>
      val state = if (x < 5) Burnt else Tree
      val vegType = if (y < 2) VegetationType.Water else VegetationType.DenseForest
      Cell(Position(x, y), state, 1500.0, vegType, 0.5, 20.0)
    }
    val grid = Grid(cells, 10, 10)
    val state = createTestState(grid)
    
    val fraction = OrderParameters.burntFraction(state)
    // 40 burnt cells out of 80 burnable cells (20 are water)
    fraction shouldBe 0.5 +- 0.01
  }
  
  "largestClusterRatio" should "find correct largest cluster" in {
    // Create a grid with two fire clusters
    val grid = createGrid(10, 10, (x, y) =>
      if ((x >= 0 && x <= 2 && y >= 0 && y <= 2) || // 3x3 cluster
          (x >= 7 && x <= 8 && y >= 7 && y <= 8))   // 2x2 cluster
        Burning
      else Tree
    )
    val state = createTestState(grid)
    
    val ratio = OrderParameters.largestClusterRatio(state)
    ratio shouldBe 0.09 +- 0.01 // 9 cells out of 100
  }
  
  "percolationIndicator" should "return 1 for system-spanning fire" in {
    // Create vertical fire strip connecting left to right
    val grid = createGrid(10, 10, (x, y) =>
      if (y == 5) Burning else Tree
    )
    val state = createTestState(grid)
    
    val indicator = OrderParameters.percolationIndicator(state)
    indicator shouldBe 1.0
  }
  
  it should "return 0 for non-spanning fire" in {
    // Create isolated fire cluster
    val grid = createGrid(10, 10, (x, y) =>
      if (x >= 4 && x <= 6 && y >= 4 && y <= 6) Burning else Tree
    )
    val state = createTestState(grid)
    
    val indicator = OrderParameters.percolationIndicator(state)
    indicator shouldBe 0.0
  }
  
  "burnVelocity" should "calculate rate of spread" in {
    val grid1 = createGrid(10, 10, (x, y) =>
      if (x < 3) Burnt else Tree
    )
    val grid2 = createGrid(10, 10, (x, y) =>
      if (x < 5) Burnt else Tree
    )
    
    val state1 = createTestState(grid1).copy(elapsedTime = 0.0)
    val state2 = createTestState(grid2).copy(elapsedTime = 10.0)
    
    val velocity = OrderParameters.burnVelocity(List(state1, state2))
    velocity shouldBe 0.02 +- 0.001 // (0.5 - 0.3) / 10
  }
  
  "clusterDensity" should "count number of clusters" in {
    // Create grid with 3 separate fire clusters
    val grid = createGrid(10, 10, (x, y) =>
      if ((x == 1 && y == 1) || 
          (x == 5 && y == 5) ||
          (x == 8 && y == 8)) Burning
      else Tree
    )
    val state = createTestState(grid)
    
    val density = OrderParameters.clusterDensity(state)
    density shouldBe 0.03 +- 0.001 // 3 clusters / 100 cells
  }
  
  "averageClusterSize" should "calculate mean cluster size" in {
    // Two clusters: one 2x2, one 1x1
    val grid = createGrid(10, 10, (x, y) =>
      if ((x >= 1 && x <= 2 && y >= 1 && y <= 2) || // 4 cells
          (x == 8 && y == 8))                        // 1 cell
        Burning
      else Tree
    )
    val state = createTestState(grid)
    
    val avgSize = OrderParameters.averageClusterSize(state)
    avgSize shouldBe 2.5 +- 0.01 // (4 + 1) / 2
  }
  
  "calculateAll" should "return all order parameters" in {
    val grid = createGrid(10, 10, (_, _) => Tree)
    val state = createTestState(grid)
    
    val allParams = OrderParameters.calculateAll(state)
    
    allParams should contain key "burntFraction"
    allParams should contain key "largestClusterRatio"
    allParams should contain key "percolationIndicator"
    allParams should contain key "clusterDensity"
    
    allParams.values.foreach { value =>
      value should be >= 0.0
      value.isNaN shouldBe false
      value.isInfinite shouldBe false
    }
  }
}