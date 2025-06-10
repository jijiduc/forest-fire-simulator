package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GridSpec extends AnyFlatSpec with Matchers {
  
  def createTestGrid(width: Int, height: Int): Grid = {
    val cells = Vector.tabulate(height, width) { (y, x) =>
      Cell(
        position = Position(x, y),
        state = if ((x + y) % 2 == 0) Tree else Empty,
        elevation = 1000.0,
        vegetationType = VegetationType.SparseForest,
        moisture = 0.5,
        temperature = 20.0
      )
    }
    Grid(cells, width, height)
  }
  
  "A Grid" should "correctly retrieve cells by coordinates" in {
    val grid = createTestGrid(3, 3)
    
    grid(0, 0).state shouldBe Tree
    grid(1, 0).state shouldBe Empty
    grid(0, 1).state shouldBe Empty
    grid(1, 1).state shouldBe Tree
  }
  
  it should "return None for out-of-bounds coordinates" in {
    val grid = createTestGrid(3, 3)
    
    grid.get(-1, 0) shouldBe None
    grid.get(0, -1) shouldBe None
    grid.get(3, 0) shouldBe None
    grid.get(0, 3) shouldBe None
  }
  
  it should "correctly update cells" in {
    val grid = createTestGrid(3, 3)
    val newCell = grid(1, 1).copy(state = Burning)
    val updatedGrid = grid.updated(1, 1, newCell)
    
    updatedGrid(1, 1).state shouldBe Burning
    grid(1, 1).state shouldBe Tree // Original grid unchanged
  }
  
  it should "correctly map over all cells" in {
    val grid = createTestGrid(3, 3)
    val allBurningGrid = grid.map(_.copy(state = Burning))
    
    allBurningGrid.cellsWithPosition.foreach { case (_, _, cell) =>
      cell.state shouldBe Burning
    }
  }
  
  it should "correctly find neighbors" in {
    val grid = createTestGrid(3, 3)
    
    // Corner cell (0, 0) has 3 neighbors
    val cornerNeighbors = grid.neighbors(0, 0)
    cornerNeighbors.length shouldBe 3
    
    // Center cell (1, 1) has 8 neighbors
    val centerNeighbors = grid.neighbors(1, 1)
    centerNeighbors.length shouldBe 8
    
    // Edge cell (1, 0) has 5 neighbors
    val edgeNeighbors = grid.neighbors(1, 0)
    edgeNeighbors.length shouldBe 5
  }
  
  it should "return cells with positions correctly" in {
    val grid = createTestGrid(2, 2)
    val cellsWithPos = grid.cellsWithPosition.toList
    
    cellsWithPos.length shouldBe 4
    cellsWithPos should contain ((0, 0, grid(0, 0)))
    cellsWithPos should contain ((1, 0, grid(1, 0)))
    cellsWithPos should contain ((0, 1, grid(0, 1)))
    cellsWithPos should contain ((1, 1, grid(1, 1)))
  }
}