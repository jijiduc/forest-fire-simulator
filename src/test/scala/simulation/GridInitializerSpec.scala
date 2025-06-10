package simulation

import models._
import terrain.TerrainGenerator
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GridInitializerSpec extends AnyFlatSpec with Matchers {
  
  def createTestSetup(): (Terrain, Climate) = {
    val terrain = TerrainGenerator.generateTerrain(50, 50, 12345L)
    val climate = Climate(
      season = Season.Summer,
      wind = Wind(0, 5),
      humidity = 0.5,
      precipitation = 0.0
    )
    (terrain, climate)
  }
  
  "GridInitializer" should "create grid with correct dimensions" in {
    val (terrain, climate) = createTestSetup()
    val grid = GridInitializer.initializeGrid(terrain, climate)
    
    grid.width shouldBe terrain.width
    grid.height shouldBe terrain.height
  }
  
  it should "assign correct positions to cells" in {
    val (terrain, climate) = createTestSetup()
    val grid = GridInitializer.initializeGrid(terrain, climate)
    
    grid.cellsWithPosition.foreach { case (x, y, cell) =>
      cell.position shouldBe Position(x, y)
    }
  }
  
  it should "assign elevations matching terrain" in {
    val (terrain, climate) = createTestSetup()
    val grid = GridInitializer.initializeGrid(terrain, climate)
    
    grid.cellsWithPosition.foreach { case (x, y, cell) =>
      cell.elevation shouldBe terrain.elevationAt(x, y)
    }
  }
  
  it should "assign vegetation types based on elevation" in {
    val (terrain, climate) = createTestSetup()
    val grid = GridInitializer.initializeGrid(terrain, climate)
    
    grid.cellsWithPosition.foreach { case (x, y, cell) =>
      cell.vegetationType shouldBe terrain.vegetationTypeAt(x, y)
    }
  }
  
  it should "create no trees above snow line" in {
    val (terrain, climate) = createTestSetup()
    val grid = GridInitializer.initializeGrid(terrain, climate)
    
    grid.cellsWithPosition.foreach { case (_, _, cell) =>
      if (cell.elevation > climate.season.snowLineElevation) {
        cell.state should not be Tree
      }
    }
  }
  
  it should "assign appropriate temperatures based on elevation" in {
    val (terrain, climate) = createTestSetup()
    val grid = GridInitializer.initializeGrid(terrain, climate)
    
    grid.cellsWithPosition.foreach { case (_, _, cell) =>
      val expectedTemp = climate.temperatureAtElevation(cell.elevation)
      cell.temperature shouldBe expectedTemp
    }
  }
  
  it should "assign moisture values within valid range" in {
    val (terrain, climate) = createTestSetup()
    val grid = GridInitializer.initializeGrid(terrain, climate)
    
    grid.cellsWithPosition.foreach { case (_, _, cell) =>
      cell.moisture should be >= 0.0
      cell.moisture should be <= 1.0
    }
  }
  
  it should "create consistent grids with same seed" in {
    val (terrain, climate) = createTestSetup()
    val seed = 54321L
    
    val grid1 = GridInitializer.initializeGrid(terrain, climate, seed)
    val grid2 = GridInitializer.initializeGrid(terrain, climate, seed)
    
    grid1.cellsWithPosition.zip(grid2.cellsWithPosition).foreach {
      case ((x1, y1, cell1), (x2, y2, cell2)) =>
        x1 shouldBe x2
        y1 shouldBe y2
        cell1 shouldBe cell2
    }
  }
  
  it should "create more trees in forested areas than alpine meadows" in {
    val (terrain, climate) = createTestSetup()
    val grid = GridInitializer.initializeGrid(terrain, climate)
    
    val forestCells = grid.cellsWithPosition.filter(_._3.vegetationType == VegetationType.SparseForest)
    val meadowCells = grid.cellsWithPosition.filter(_._3.vegetationType == VegetationType.Grassland)
    
    if (forestCells.nonEmpty && meadowCells.nonEmpty) {
      val forestTreeRatio = forestCells.count(_._3.state == Tree).toDouble / forestCells.length
      val meadowTreeRatio = meadowCells.count(_._3.state == Tree).toDouble / meadowCells.length
      
      forestTreeRatio should be > meadowTreeRatio
    }
  }
}