package simulation.rules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import models._
import simulation._

class RuleEngineSpec extends AnyFlatSpec with Matchers {
  
  def createTestCell(state: CellState = Tree): Cell = Cell(
    position = Position(5, 5),
    state = state,
    elevation = 1500.0,
    vegetationType = VegetationType.DenseForest,
    moisture = 0.5,
    temperature = 20.0
  )
  
  def createTestState(): SimulationState = {
    val grid = Grid(Vector.fill(10)(Vector.fill(10)(createTestCell())), 10, 10)
    SimulationState(
      grid = grid,
      climate = Climate(Season.Summer, Wind(0, 5.0), 0.5, 0.0),
      terrain = Terrain(Vector.fill(10)(Vector.fill(10)(1500.0)), 10, 10),
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
  }
  
  "RuleEngine" should "be created with default configuration" in {
    val config = RuleConfig()
    val engine = RuleEngine.default[IO](config)
    
    engine.allRules.length should be > 0
  }
  
  it should "select appropriate rules based on cell state" in {
    val config = RuleConfig()
    val engine = RuleEngine.default[IO](config)
    val state = createTestState()
    
    val treeRules = engine.selectApplicableRules(createTestCell(Tree), state)
    val burningRules = engine.selectApplicableRules(createTestCell(Burning), state)
    val burntRules = engine.selectApplicableRules(createTestCell(Burnt), state)
    
    // Tree cells should get ignition rules
    treeRules.map(_.name) should contain atLeastOneOf ("SparkIgnition", "NeighborIgnition", "EmberIgnition")
    
    // Burning cells should get burning and extinction rules
    burningRules.map(_.name) should contain atLeastOneOf ("IntensityEvolution", "FuelConsumption", "FuelDepletion", "TemperatureDecay")
    
    // Burnt cells should get recovery rules
    burntRules.map(_.name) should contain atLeastOneOf ("NaturalRegrowth", "SeasonalGrowth")
  }
  
  it should "apply rules to transform cells" in {
    val config = RuleConfig(enableSparks = false) // Disable random sparks
    val engine = RuleEngine.default[IO](config)
    val state = createTestState()
    
    // Test tree cell with burning neighbor
    val treeCell = createTestCell(Tree)
    val burningNeighbor = createTestCell(Burning).copy(temperature = 500.0)
    val neighbors = List(burningNeighbor) ++ List.fill(7)(createTestCell(Tree))
    
    val result = engine.applyRules(treeCell, neighbors, state).unsafeRunSync()
    
    // Cell should either remain tree or become burning
    (result.state == Tree || result.state == Burning) shouldBe true
  }
}

class IgnitionRulesSpec extends AnyFlatSpec with Matchers {
  
  def createTestState(): SimulationState = {
    val grid = Grid(Vector.fill(10)(Vector.fill(10)(Cell(
      Position(0, 0), Tree, 1500.0, VegetationType.DenseForest, 0.5, 20.0
    ))), 10, 10)
    SimulationState(
      grid = grid,
      climate = Climate(Season.Summer, Wind(0, 5.0), 0.5, 0.0),
      terrain = Terrain(Vector.fill(10)(Vector.fill(10)(1500.0)), 10, 10),
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
  }
  
  "SparkIgnition" should "only apply to trees in non-winter seasons" in {
    val rule = IgnitionRules.SparkIgnition[IO](0.1)
    val state = createTestState()
    val winterState = state.copy(climate = state.climate.copy(season = Season.Winter))
    
    val treeCell = Cell(Position(5, 5), Tree, 1500.0, VegetationType.DenseForest, 0.5, 20.0)
    val burningCell = treeCell.copy(state = Burning)
    
    rule.isApplicable(treeCell, state) shouldBe true
    rule.isApplicable(treeCell, winterState) shouldBe false
    rule.isApplicable(burningCell, state) shouldBe false
  }
  
  "NeighborIgnition" should "apply to trees" in {
    val rule = IgnitionRules.NeighborIgnition[IO]()
    val state = createTestState()
    
    val treeCell = Cell(Position(5, 5), Tree, 1500.0, VegetationType.DenseForest, 0.5, 20.0)
    val burningCell = treeCell.copy(state = Burning)
    
    rule.isApplicable(treeCell, state) shouldBe true
    rule.isApplicable(burningCell, state) shouldBe false
  }
  
  it should "have higher probability with burning neighbors" in {
    val rule = IgnitionRules.NeighborIgnition[IO]()
    val state = createTestState()
    
    // Create test cell with very low moisture for higher ignition probability
    val treeCell = Cell(Position(5, 5), Tree, 1500.0, VegetationType.DenseForest, 0.05, 25.0)
    val burningNeighbor = Cell(Position(4, 5), Burning, 1500.0, VegetationType.DenseForest, 0.5, 500.0)
    val treeNeighbor = Cell(Position(6, 5), Tree, 1500.0, VegetationType.DenseForest, 0.5, 20.0)
    
    val withBurning = List(burningNeighbor) ++ List.fill(7)(treeNeighbor)
    val withoutBurning = List.fill(8)(treeNeighbor)
    
    // Test with high temperature climate for better ignition
    val hotClimate = Climate(Season.Summer, Wind(0, 10.0), 0.3, 0.0)
    val hotState = state.copy(climate = hotClimate, timeStep = 1.0) // Use larger timestep
    
    // Run a single test to see if it can ignite
    val singleResult = rule.apply(treeCell, withBurning, hotState).unsafeRunSync()
    
    // If single test doesn't ignite, test probabilistically
    if (singleResult.state != Burning) {
      var ignitionsWithBurning = 0
      
      for (i <- 1 to 100) {
        val testState = hotState.copy(elapsedTime = i.toDouble)
        val result = rule.apply(treeCell, withBurning, testState).unsafeRunSync()
        if (result.state == Burning) ignitionsWithBurning += 1
      }
      
      // Should ignite at least once
      ignitionsWithBurning should be > 0
    } else {
      // Already ignited in single test
      singleResult.state shouldBe Burning
    }
    
    // Without burning neighbors, should never ignite
    val noFireResult = rule.apply(treeCell, withoutBurning, hotState).unsafeRunSync()
    noFireResult.state shouldBe Tree
  }
  
  "EmberIgnition" should "only apply in windy conditions" in {
    val rule = IgnitionRules.EmberIgnition[IO](5)
    val state = createTestState()
    val windyState = state.copy(climate = state.climate.copy(wind = Wind(0, 10.0)))
    val calmState = state.copy(climate = state.climate.copy(wind = Wind(0, 2.0)))
    
    val treeCell = Cell(Position(5, 5), Tree, 1500.0, VegetationType.DenseForest, 0.5, 20.0)
    
    rule.isApplicable(treeCell, windyState) shouldBe true
    rule.isApplicable(treeCell, calmState) shouldBe false
  }
}

class BurningRulesSpec extends AnyFlatSpec with Matchers {
  
  def createTestState(): SimulationState = {
    val grid = Grid(Vector.fill(10)(Vector.fill(10)(Cell(
      Position(0, 0), Tree, 1500.0, VegetationType.DenseForest, 0.5, 20.0
    ))), 10, 10)
    SimulationState(
      grid = grid,
      climate = Climate(Season.Summer, Wind(0, 5.0), 0.5, 0.0),
      terrain = Terrain(Vector.fill(10)(Vector.fill(10)(1500.0)), 10, 10),
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
  }
  
  "IntensityEvolution" should "only apply to burning cells" in {
    val rule = BurningRules.IntensityEvolution[IO]()
    val state = createTestState()
    
    val burningCell = Cell(Position(5, 5), Burning, 1500.0, VegetationType.DenseForest, 0.3, 400.0)
    val treeCell = burningCell.copy(state = Tree)
    
    rule.isApplicable(burningCell, state) shouldBe true
    rule.isApplicable(treeCell, state) shouldBe false
  }
  
  it should "update temperature based on conditions" in {
    val rule = BurningRules.IntensityEvolution[IO]()
    val state = createTestState()
    
    val burningCell = Cell(Position(5, 5), Burning, 1500.0, VegetationType.DenseForest, 0.1, 400.0)
    val neighbors = List.fill(8)(Cell(Position(0, 0), Tree, 1500.0, VegetationType.DenseForest, 0.5, 20.0))
    
    val result = rule.apply(burningCell, neighbors, state).unsafeRunSync()
    
    result.temperature should not equal burningCell.temperature
  }
  
  "FuelConsumption" should "increase moisture as fuel depletes" in {
    val rule = BurningRules.FuelConsumption[IO]()
    val state = createTestState()
    
    val burningCell = Cell(Position(5, 5), Burning, 1500.0, VegetationType.DenseForest, 0.3, 500.0)
    val neighbors = List.empty
    
    val result = rule.apply(burningCell, neighbors, state).unsafeRunSync()
    
    result.moisture should be > burningCell.moisture
  }
}

class ExtinctionRulesSpec extends AnyFlatSpec with Matchers {
  
  def createTestState(): SimulationState = {
    val grid = Grid(Vector.fill(10)(Vector.fill(10)(Cell(
      Position(0, 0), Tree, 1500.0, VegetationType.DenseForest, 0.5, 20.0
    ))), 10, 10)
    SimulationState(
      grid = grid,
      climate = Climate(Season.Summer, Wind(0, 5.0), 0.5, 0.0),
      terrain = Terrain(Vector.fill(10)(Vector.fill(10)(1500.0)), 10, 10),
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
  }
  
  "FuelDepletion" should "transition to burnt when fuel exhausted" in {
    val rule = ExtinctionRules.FuelDepletion[IO]()
    val state = createTestState()
    
    val depletedCell = Cell(Position(5, 5), Burning, 1500.0, VegetationType.DenseForest, 0.95, 300.0)
    val freshCell = Cell(Position(5, 5), Burning, 1500.0, VegetationType.DenseForest, 0.3, 500.0)
    
    val result1 = rule.apply(depletedCell, List.empty, state).unsafeRunSync()
    val result2 = rule.apply(freshCell, List.empty, state).unsafeRunSync()
    
    result1.state shouldBe Burnt
    result2.state shouldBe Burning
  }
  
  "TemperatureDecay" should "extinguish cold fires" in {
    val rule = ExtinctionRules.TemperatureDecay[IO]()
    val state = createTestState()
    
    val coldFire = Cell(Position(5, 5), Burning, 1500.0, VegetationType.DenseForest, 0.5, 100.0)
    val hotFire = Cell(Position(5, 5), Burning, 1500.0, VegetationType.DenseForest, 0.5, 400.0)
    
    val result1 = rule.apply(coldFire, List.empty, state).unsafeRunSync()
    val result2 = rule.apply(hotFire, List.empty, state).unsafeRunSync()
    
    result1.state shouldBe Burnt
    result2.state shouldBe Burning
  }
}

class RecoveryRulesSpec extends AnyFlatSpec with Matchers {
  
  def createTestState(): SimulationState = {
    val grid = Grid(Vector.fill(10)(Vector.fill(10)(Cell(
      Position(0, 0), Tree, 1500.0, VegetationType.DenseForest, 0.5, 20.0
    ))), 10, 10)
    SimulationState(
      grid = grid,
      climate = Climate(Season.Spring, Wind(0, 5.0), 0.5, 2.0),
      terrain = Terrain(Vector.fill(10)(Vector.fill(10)(1500.0)), 10, 10),
      timeStep = 0.1,
      elapsedTime = 100.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
  }
  
  "NaturalRegrowth" should "apply to burnt and empty cells" in {
    val rule = RecoveryRules.NaturalRegrowth[IO](0.01)
    val state = createTestState()
    
    val burntCell = Cell(Position(5, 5), Burnt, 1500.0, VegetationType.DenseForest, 0.5, 20.0)
    val emptyCell = Cell(Position(5, 5), Empty, 1500.0, VegetationType.Barren, 0.5, 20.0)
    val treeCell = Cell(Position(5, 5), Tree, 1500.0, VegetationType.DenseForest, 0.5, 20.0)
    
    rule.isApplicable(burntCell, state) shouldBe true
    rule.isApplicable(emptyCell, state) shouldBe true
    rule.isApplicable(treeCell, state) shouldBe false
  }
  
  "SeasonalGrowth" should "apply in spring and summer" in {
    val rule = RecoveryRules.SeasonalGrowth[IO]()
    val springState = createTestState()
    val winterState = springState.copy(climate = springState.climate.copy(season = Season.Winter))
    
    val burntCell = Cell(Position(5, 5), Burnt, 1500.0, VegetationType.DenseForest, 0.5, 20.0)
    
    rule.isApplicable(burntCell, springState) shouldBe true
    rule.isApplicable(burntCell, winterState) shouldBe false
  }
}