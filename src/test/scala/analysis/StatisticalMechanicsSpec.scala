package analysis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.effect._
import cats.effect.unsafe.implicits.global
import models._
import simulation._

class StatisticalMechanicsSpec extends AnyFlatSpec with Matchers {
  
  "CriticalExponentAnalyzer" should "extract beta exponent from order parameter data" in {
    // Create synthetic data with known exponent
    val criticalPoint = 0.5
    val beta = 0.139 // 2D percolation
    
    val data = (-10 to 10).filter(_ != 0).map { i =>
      val p = criticalPoint + i * 0.01
      val t = math.abs(p - criticalPoint)
      // Only use data above critical point for beta extraction
      val m = if (p > criticalPoint) math.pow(t, beta) else 0.0
      
      val ensemble = EnsembleResults(
        results = List.empty,
        averageOrderParameters = Map("burntFraction" -> m),
        standardDeviations = Map("burntFraction" -> 0.01),
        phase = if (p < criticalPoint) SubCritical else SuperCritical
      )
      
      ParameterPoint(TreeDensityParameter, p, ensemble)
    }.toList
    
    val result = CriticalExponentAnalyzer.extractBeta(data, criticalPoint)
    
    result.value should be(beta +- 0.02)
    result.method shouldBe "log-log regression"
    result.confidence should be > 0.8
  }
  
  it should "extract gamma exponent from susceptibility data" in {
    val criticalPoint = 0.5
    val gamma = 1.79 // 2D percolation
    
    val susceptibilityData = (-10 to 10).filter(_ != 0).map { i =>
      val p = criticalPoint + i * 0.01
      val t = math.abs(p - criticalPoint)
      val chi = math.pow(t, -gamma)
      (p, chi)
    }.toList
    
    val result = CriticalExponentAnalyzer.extractGamma(susceptibilityData, criticalPoint)
    
    result.value should be(gamma +- 0.05)
  }
  
  it should "extract tau exponent from cluster distribution" in {
    // Power-law cluster distribution with better statistics
    val tau = 2.055
    val distributions = List(
      (1 to 100).map { s =>
        // Use more realistic count that ensures good statistics for small clusters
        val count = (100000 * math.pow(s.toDouble, -tau)).toInt.max(10)
        s -> count
      }.toMap
    )
    
    val result = CriticalExponentAnalyzer.extractTau(distributions)
    
    result.value should be(tau +- 0.1)
  }
  
  "FiniteSizeScaling" should "perform data collapse" in {
    val runner = new SimulationRunner[IO]()
    val fss = new FiniteSizeScalingImpl[IO](runner)
    
    // Create synthetic data that should collapse
    val pc = 0.5927 // 2D percolation threshold
    val nu = 1.33
    val beta = 0.139
    
    val sizes = List(20, 40, 80)
    val data = sizes.map { L =>
      val points = (-10 to 10).map { i =>
        val p = pc + i * 0.01 / math.pow(L, 1/nu)
        val m = math.pow(L, -beta/nu) * f((p - pc) * math.pow(L, 1/nu))
        
        val ensemble = EnsembleResults(
          results = List.empty,
          averageOrderParameters = Map("burntFraction" -> m),
          standardDeviations = Map.empty,
          phase = Critical
        )
        
        ParameterPoint(TreeDensityParameter, p, ensemble)
      }.toList
      L -> points
    }.toMap
    
    val exponents = CriticalExponents(beta = beta, nu = nu)
    val result = fss.performDataCollapse(data, exponents).unsafeRunSync()
    
    result.collapsed shouldBe true
    result.rSquared should be > 0.9
  }
  
  // Scaling function for testing
  private def f(x: Double): Double = {
    if (x > 0) math.tanh(x) else 0.0
  }
}