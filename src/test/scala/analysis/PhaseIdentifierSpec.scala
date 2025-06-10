package analysis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import models._
import simulation._

class PhaseIdentifierSpec extends AnyFlatSpec with Matchers {
  
  "PhaseIdentifier" should "identify sub-critical phase" in {
    val phase = PhaseIdentifier.identifyFromParameters(
      burntFraction = 0.05,
      largestClusterRatio = 0.02,
      percolationIndicator = 0.0
    )
    
    phase shouldBe SubCritical
  }
  
  it should "identify super-critical phase" in {
    val phase = PhaseIdentifier.identifyFromParameters(
      burntFraction = 0.7,
      largestClusterRatio = 0.6,
      percolationIndicator = 1.0
    )
    
    phase shouldBe SuperCritical
  }
  
  it should "identify critical phase" in {
    val phase = PhaseIdentifier.identifyFromParameters(
      burntFraction = 0.25,
      largestClusterRatio = 0.15,
      percolationIndicator = 0.0
    )
    
    phase shouldBe Critical
  }
  
  "criticalityScore" should "return high score near criticality" in {
    // Create a state with intermediate burnt fraction
    val grid = Grid(
      Vector.tabulate(10, 10) { (y, x) =>
        val state = if ((x + y) % 4 == 0) Burning else Tree
        Cell(Position(x, y), state, 1500.0, VegetationType.DenseForest, 0.5, 20.0)
      },
      10, 10
    )
    
    val state = SimulationState(
      grid = grid,
      climate = Climate(Season.Summer, Wind(0, 5.0), 0.5, 0.0),
      terrain = Terrain(Vector.fill(10)(Vector.fill(10)(1500.0)), 10, 10),
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
    
    val score = PhaseIdentifier.criticalityScore(state)
    score should be > 0.0
    score should be <= 1.0
  }
  
  "detectTransition" should "find transition point" in {
    val paramValues = List(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
    val orderValues = List(0.0, 0.01, 0.02, 0.05, 0.15, 0.5, 0.8, 0.9, 0.95, 0.98, 0.99)
    
    val transition = PhaseIdentifier.detectTransition(paramValues, orderValues)
    
    transition should not be empty
    transition.get should be(0.5 +- 0.1) // Steepest change around 0.5
  }
  
  it should "return None for no clear transition" in {
    val paramValues = List(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
    val orderValues = List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6) // Linear change
    
    val transition = PhaseIdentifier.detectTransition(paramValues, orderValues)
    
    transition shouldBe None
  }
  
  "identifyFromClusterDistribution" should "detect power law at criticality" in {
    // Create power-law distribution typical of critical phase
    val clusterSizes = Map(
      1 -> 100,
      2 -> 50,
      4 -> 25,
      8 -> 12,
      16 -> 6,
      32 -> 3,
      64 -> 1
    )
    
    val phase = PhaseIdentifier.identifyFromClusterDistribution(clusterSizes)
    
    phase shouldBe Critical
  }
  
  it should "identify super-critical from dominant large cluster" in {
    val clusterSizes = Map(
      1 -> 10,
      2 -> 5,
      200 -> 1 // One very large cluster
    )
    
    val phase = PhaseIdentifier.identifyFromClusterDistribution(clusterSizes)
    
    phase shouldBe SuperCritical
  }
}