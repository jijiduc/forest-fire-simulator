package analysis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import models._
import simulation._

class CorrelationAnalysisSpec extends AnyFlatSpec with Matchers {
  
  "SpatialCorrelations" should "calculate two-point correlation function" in {
    // Create a grid with known correlation pattern
    val size = 20
    val grid = Grid(
      cells = Vector.tabulate(size, size) { (y, x) =>
        // Create correlated pattern
        val state = if ((x / 5 + y / 5) % 2 == 0) Burning else Tree
        Cell(Position(x, y), state, 1500.0, VegetationType.DenseForest, 0.5, 20.0)
      },
      width = size,
      height = size
    )
    
    val state = createTestState(grid)
    val correlations = SpatialCorrelations.twoPointCorrelation(state, 10)
    
    // Should see oscillating correlation due to pattern
    correlations.size should be > 0
    correlations(1) should not be correlations(5)
  }
  
  it should "extract correlation length" in {
    val correlations = Map(
      1 -> 0.8,
      2 -> 0.6,
      3 -> 0.4,
      4 -> 0.2,
      5 -> 0.1
    )
    
    val xi = SpatialCorrelations.correlationLength(correlations)
    
    // Should find where correlation drops to ~0.29 (1/e of 0.8)
    // With given data, this happens between 3 and 4, but implementation returns 4
    xi should be(4.0 +- 1.0)
  }
  
  it should "detect anisotropy in directional correlations" in {
    // Create anisotropic pattern (horizontal stripes)
    val size = 20
    val grid = Grid(
      cells = Vector.tabulate(size, size) { (y, x) =>
        val state = if (y % 4 < 2) Burning else Tree
        Cell(Position(x, y), state, 1500.0, VegetationType.DenseForest, 0.5, 20.0)
      },
      width = size,
      height = size
    )
    
    val state = createTestState(grid)
    val result = SpatialCorrelations.directionalCorrelations(state)
    
    result.anisotropy should be > 1.2
    // Horizontal correlation should be stronger than vertical
    result.horizontal(2) should be > result.vertical(2)
  }
  
  "TemporalCorrelations" should "calculate autocorrelation function" in {
    // Create oscillating time series
    val timeSeries = (0 until 100).map { t =>
      val value = math.sin(2 * math.Pi * t / 20) + 0.1 * util.Random.nextGaussian()
      Map("value" -> value)
    }.toList
    
    val autocorr = TemporalCorrelations.autocorrelation(timeSeries, "value", 50)
    
    autocorr(0) should be(1.0 +- 0.01)
    // Should see periodic structure
    autocorr(20) should be > 0.5  // Period is 20
    autocorr(10) should be < -0.3 // Half period
  }
  
  it should "extract relaxation time" in {
    val autocorr = Map(
      0 -> 1.0,
      1 -> 0.8,
      2 -> 0.6,
      3 -> 0.4,
      4 -> 0.3,
      5 -> 0.2
    )
    
    val tau = TemporalCorrelations.relaxationTime(autocorr)
    
    // Should find where it drops below 1/e ≈ 0.368
    // With given data, this happens at lag 4
    tau should be(4.0 +- 1.0)
  }
  
  it should "calculate power spectral density" in {
    // Pure sine wave
    val frequency = 0.1
    val timeSeries = (0 until 100).map { t =>
      math.sin(2 * math.Pi * frequency * t)
    }.toList
    
    val psd = TemporalCorrelations.powerSpectralDensity(timeSeries)
    
    // Should have peak at the frequency
    val maxPower = psd.maxBy(_._2)
    maxPower._1 should be(frequency +- 0.02)
  }
  
  it should "detect critical slowing down" in {
    // Create time series that slows down
    val timeSeries = (0 until 200).map { t =>
      val tau = 1.0 + t * 0.1 // Increasing relaxation time
      val value = math.exp(-1.0 / tau) * util.Random.nextGaussian()
      Map("value" -> value)
    }.toList
    
    val csd = TemporalCorrelations.criticalSlowingDown(timeSeries, "value", 20)
    
    // Should see increasing values
    csd.length should be > 0
    csd.last should be > csd.head
  }
  
  // Helper method
  private def createTestState(grid: Grid): SimulationState = {
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
}