package analysis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import models._
import simulation._
import terrain.TerrainGenerator

class PhaseParameterSpec extends AnyFlatSpec with Matchers {
  
  def createTestState(): SimulationState = {
    val terrain = TerrainGenerator.generateTerrain(10, 10, 42L)
    val climate = Climate(Season.Summer, Wind(0, 5.0), 0.5, 0.0)
    val grid = GridInitializer.initializeGrid(terrain, climate, 42L)
    
    SimulationState(
      grid = grid,
      climate = climate,
      terrain = terrain,
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
  }
  
  "TreeDensityParameter" should "modify tree density in grid" in {
    val state = createTestState()
    val density = 0.7
    
    val modifiedState = TreeDensityParameter.applyToState(state, density)
    
    // Count trees
    val cells = modifiedState.grid.cells.flatten
    val treeCount = cells.count(_.state == Tree)
    val totalNonWater = cells.count(_.vegetationType != VegetationType.Water)
    
    val actualDensity = treeCount.toDouble / totalNonWater
    actualDensity should be(density +- 0.1) // Allow some variance
  }
  
  it should "not modify burning or burnt cells" in {
    val state = createTestState()
    // Set some cells as burning/burnt
    val gridWithFire = state.grid
      .updated(5, 5, state.grid(5, 5).copy(state = Burning))
      .updated(6, 6, state.grid(6, 6).copy(state = Burnt))
    
    val stateWithFire = state.copy(grid = gridWithFire)
    val modifiedState = TreeDensityParameter.applyToState(stateWithFire, 0.5)
    
    modifiedState.grid(5, 5).state shouldBe Burning
    modifiedState.grid(6, 6).state shouldBe Burnt
  }
  
  "MoistureParameter" should "set moisture for all cells" in {
    val state = createTestState()
    val moisture = 0.3
    
    val modifiedState = MoistureParameter.applyToState(state, moisture)
    
    modifiedState.grid.cells.flatten.foreach { cell =>
      cell.moisture shouldBe moisture
    }
  }
  
  "WindSpeedParameter" should "modify wind speed in climate" in {
    val state = createTestState()
    val windSpeed = 15.0
    
    val modifiedState = WindSpeedParameter.applyToState(state, windSpeed)
    
    modifiedState.climate.wind.speed shouldBe windSpeed
    modifiedState.climate.wind.direction shouldBe state.climate.wind.direction
  }
  
  "TemperatureParameter" should "modify cell temperatures" in {
    val state = createTestState()
    val tempAnomaly = 5.0
    val baseTemp = state.climate.season.baseTemperature
    
    val modifiedState = TemperatureParameter.applyToState(state, tempAnomaly)
    
    // Check that temperatures increased
    modifiedState.grid.cells.flatten.foreach { cell =>
      val expectedTemp = state.climate.temperatureAtElevation(cell.elevation) + tempAnomaly
      cell.temperature shouldBe expectedTemp +- 0.1
    }
  }
  
  "ParameterRange" should "generate correct values" in {
    val range = ParameterRange(0.0, 1.0, 11)
    val values = range.values
    
    values.length shouldBe 11
    values.head shouldBe 0.0
    values.last shouldBe 1.0
    
    // Check uniform spacing
    for (i <- 1 until values.length) {
      (values(i) - values(i-1)) shouldBe 0.1 +- 0.001
    }
  }
  
  it should "handle single step" in {
    val range = ParameterRange(0.5, 0.5, 1)
    range.values shouldBe List(0.5)
  }
}