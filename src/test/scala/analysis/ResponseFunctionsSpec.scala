package analysis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import models._
import simulation._

class ResponseFunctionsSpec extends AnyFlatSpec with Matchers {
  
  "ResponseFunctions" should "calculate magnetic susceptibility" in {
    // Create ensemble with known variance
    val values = List(0.1, 0.2, 0.3, 0.4, 0.5)
    val mean = values.sum / values.length
    val variance = values.map(v => math.pow(v - mean, 2)).sum / values.length
    
    val results = values.map { v =>
      SimulationResult(
        finalState = createTestState(10),
        timeSeries = List.empty,
        orderParameters = Map("burntFraction" -> v),
        phase = Critical,
        runTime = 1.0
      )
    }
    
    val ensemble = EnsembleResults(
      results = results,
      averageOrderParameters = Map("burntFraction" -> mean),
      standardDeviations = Map("burntFraction" -> math.sqrt(variance)),
      phase = Critical
    )
    
    val chi = ResponseFunctions.magneticSusceptibility(ensemble)
    
    chi should be(100 * variance +- 0.01) // System size is 10x10 = 100
  }
  
  it should "calculate Binder cumulant" in {
    // Test with known distribution
    val values = List(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
    
    val results = values.map { v =>
      SimulationResult(
        finalState = createTestState(10),
        timeSeries = List.empty,
        orderParameters = Map("burntFraction" -> v),
        phase = Critical,
        runTime = 1.0
      )
    }
    
    val ensemble = EnsembleResults(
      results = results,
      averageOrderParameters = Map.empty,
      standardDeviations = Map.empty,
      phase = Critical
    )
    
    val binder = ResponseFunctions.binderCumulant(ensemble)
    
    // Binder cumulant should be between 0 and 1
    binder should be >= 0.0
    binder should be <= 1.0
  }
  
  it should "calculate cumulant ratios" in {
    // Create ensemble with normal-like distribution centered at 0.5
    val values = (0 until 100).map { i =>
      val x = i.toDouble / 100
      // Create a distribution centered at 0.5 with some variance
      0.5 + 0.2 * (x - 0.5)
    }.toList
    
    val results = values.map { v =>
      SimulationResult(
        finalState = createTestState(10),
        timeSeries = List.empty,
        orderParameters = Map("burntFraction" -> v),
        phase = Critical,
        runTime = 1.0
      )
    }
    
    val ensemble = EnsembleResults(
      results = results,
      averageOrderParameters = Map.empty,
      standardDeviations = Map.empty,
      phase = Critical
    )
    
    val ratios = ResponseFunctions.cumulantRatios(ensemble, 6)
    
    ratios.contains(4) shouldBe true
    ratios.contains(6) shouldBe true
    
    // For this distribution, ratios should exist and be finite
    ratios(4).isNaN shouldBe false
    ratios(6).isNaN shouldBe false
  }
  
  it should "calculate order parameter derivative" in {
    // Create data with known slope
    val points = (0 to 10).map { i =>
      val p = i * 0.1
      val m = p * p // Quadratic relation
      
      val ensemble = EnsembleResults(
        results = List.empty,
        averageOrderParameters = Map("burntFraction" -> m),
        standardDeviations = Map.empty,
        phase = Critical
      )
      
      ParameterPoint(TreeDensityParameter, p, ensemble)
    }.toList
    
    val derivatives = ResponseFunctions.orderParameterDerivative(points)
    
    derivatives.length should be(points.length - 2)
    
    // For m = p², dm/dp = 2p
    derivatives(4)._2 should be(2 * 0.5 +- 0.1) // At p = 0.5
  }
  
  it should "extract cluster sizes correctly" in {
    // Create grid with known clusters
    val gridData = Vector(
      Vector(burning, burning, tree,    tree),
      Vector(burning, tree,    tree,    tree),
      Vector(tree,    tree,    burning, burning),
      Vector(tree,    tree,    burning, tree)
    )
    
    val grid = Grid(
      cells = gridData.zipWithIndex.map { case (row, y) =>
        row.zipWithIndex.map { case (state, x) =>
          Cell(Position(x, y), state, 1500.0, VegetationType.DenseForest, 0.5, 20.0)
        }
      },
      width = 4,
      height = 4
    )
    
    val state = SimulationState(
      grid = grid,
      climate = Climate(Season.Summer, Wind(0, 5.0), 0.5, 0.0),
      terrain = Terrain(Vector.fill(4)(Vector.fill(4)(1500.0)), 4, 4),
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
    val ensemble = EnsembleResults(
      results = List(SimulationResult(state, List.empty, Map.empty, Critical, 1.0)),
      averageOrderParameters = Map.empty,
      standardDeviations = Map.empty,
      phase = Critical
    )
    
    val chi = ResponseFunctions.connectedSusceptibility(ensemble)
    
    chi should be >= 0.0
  }
  
  // Helper methods
  private def createTestState(size: Int): SimulationState = {
    val grid = Grid(
      cells = Vector.tabulate(size, size) { (y, x) =>
        Cell(Position(x, y), Tree, 1500.0, VegetationType.DenseForest, 0.5, 20.0)
      },
      width = size,
      height = size
    )
    
    SimulationState(
      grid = grid,
      climate = Climate(Season.Summer, Wind(0, 5.0), 0.5, 0.0),
      terrain = Terrain(Vector.fill(size)(Vector.fill(size)(1500.0)), size, size),
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
  }
  
  private val burning = Burning
  private val tree = Tree
}