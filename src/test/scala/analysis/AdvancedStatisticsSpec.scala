package analysis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AdvancedStatisticsSpec extends AnyFlatSpec with Matchers {
  
  "AdvancedStatistics.bootstrap" should "estimate standard error correctly" in {
    val data = List.fill(100)(util.Random.nextGaussian())
    val mean: List[Double] => Double = d => d.sum / d.length
    
    val result = AdvancedStatistics.bootstrap(data, mean, nSamples = 1000)
    
    // Standard error of mean should be approximately σ/√n
    val expectedSE = 1.0 / math.sqrt(100)
    result.standardError should be(expectedSE +- 0.05)
    
    // Check confidence interval contains true mean (0)
    result.confidenceInterval._1 should be < 0.0
    result.confidenceInterval._2 should be > 0.0
  }
  
  it should "detect bias in estimators" in {
    val data = (1 to 100).map(_.toDouble).toList
    val trueEstimate = data.sum / data.length
    val biasedEstimator: List[Double] => Double = d => {
      val mean = d.sum / d.length
      mean * 0.9 // 10% bias
    }
    
    val result = AdvancedStatistics.bootstrap(data, biasedEstimator, nSamples = 500)
    
    // The bootstrap estimate should be close to the biased value
    val biasedValue = trueEstimate * 0.9
    result.estimate should be(biasedValue +- 2.0)
    
    // Just check that bias is calculated (positive or negative)
    result.bias.abs should be > 0.0
  }
  
  "AdvancedStatistics.jackknife" should "reduce bias" in {
    val data = List.tabulate(50)(i => i.toDouble)
    val variance: List[Double] => Double = d => {
      val mean = d.sum / d.length
      d.map(x => math.pow(x - mean, 2)).sum / d.length // Biased estimator
    }
    
    val result = AdvancedStatistics.jackknife(data, variance)
    
    // Jackknife should produce less biased estimate
    val trueVariance = data.map(x => math.pow(x - data.sum / data.length, 2)).sum / (data.length - 1)
    result.estimate should be(trueVariance +- 1.0)
  }
  
  "AdvancedStatistics.maximumLikelihoodExponent" should "estimate power-law exponent" in {
    // Generate power-law data
    val alpha = 2.5
    val xMin = 1.0
    val data = (1 to 1000).map { _ =>
      val u = util.Random.nextDouble()
      xMin * math.pow(1 - u, -1.0 / (alpha - 1))
    }.toList
    
    val result = AdvancedStatistics.maximumLikelihoodExponent(data, xMin)
    
    result.estimate should be(alpha +- 0.1)
    result.converged shouldBe true
    result.standardError should be < 0.1
  }
  
  "AdvancedStatistics.kolmogorovSmirnovTest" should "test goodness of fit" in {
    // Test uniform distribution
    val data = List.fill(100)(util.Random.nextDouble())
    val uniformCDF: Double => Double = x => x.max(0).min(1)
    
    val (dStatistic, pValue) = AdvancedStatistics.kolmogorovSmirnovTest(data, uniformCDF)
    
    dStatistic should be < 0.2
    pValue should be > 0.05 // Should not reject null hypothesis
  }
  
  it should "reject bad fits" in {
    // Test normal data against uniform CDF
    val data = List.fill(100)(util.Random.nextGaussian() * 0.1 + 0.5)
    val uniformCDF: Double => Double = x => x.max(0).min(1)
    
    val (dStatistic, pValue) = AdvancedStatistics.kolmogorovSmirnovTest(data, uniformCDF)
    
    pValue should be < 0.05 // Should reject
  }
  
  "AdvancedStatistics.weightedLeastSquares" should "fit with errors" in {
    // Generate data with known slope and varying errors
    val trueSlope = 2.0
    val trueIntercept = 1.0
    
    val data = (1 to 20).map { x =>
      val error = 0.1 * x // Larger error at higher x
      val y = trueSlope * x + trueIntercept + util.Random.nextGaussian() * error
      (x.toDouble, y, error)
    }.toList
    
    val (slope, intercept, chiSquared) = AdvancedStatistics.weightedLeastSquares(data)
    
    slope should be(trueSlope +- 0.2)
    intercept should be(trueIntercept +- 0.5)
    chiSquared should be < 50.0 // Reasonable fit
  }
  
  "Information criteria" should "penalize complexity" in {
    val logLikelihood = -100.0
    val nData = 100
    
    val aic1 = AdvancedStatistics.akaikeInformationCriterion(logLikelihood, 2)
    val aic2 = AdvancedStatistics.akaikeInformationCriterion(logLikelihood, 5)
    
    aic2 should be > aic1 // More parameters = higher AIC
    
    val bic1 = AdvancedStatistics.bayesianInformationCriterion(logLikelihood, 2, nData)
    val bic2 = AdvancedStatistics.bayesianInformationCriterion(logLikelihood, 5, nData)
    
    bic2 should be > bic1 // More parameters = higher BIC
  }
  
  "AdvancedStatistics.crossValidation" should "evaluate models" in {
    val data = (1 to 100).map(i => (i.toDouble, 2 * i + util.Random.nextGaussian())).toList
    
    def trainAndEvaluate(train: List[(Double, Double)], test: List[(Double, Double)]): Double = {
      // Simple linear regression
      val n = train.length
      val sumX = train.map(_._1).sum
      val sumY = train.map(_._2).sum
      val sumXY = train.map(p => p._1 * p._2).sum
      val sumX2 = train.map(p => p._1 * p._1).sum
      
      val slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
      val intercept = (sumY - slope * sumX) / n
      
      // Calculate MSE on test set
      val mse = test.map { case (x, y) =>
        val pred = slope * x + intercept
        math.pow(y - pred, 2)
      }.sum / test.length
      
      -mse // Negative because we want to maximize
    }
    
    val (score, error) = AdvancedStatistics.crossValidation(data, 5, trainAndEvaluate)
    
    score should be < -0.5 // Should have small MSE
    error should be > 0.0
  }
  
  "AdvancedStatistics.blockingAnalysis" should "find optimal block size" in {
    // Create correlated time series
    val n = 1000
    var current = 0.0
    val timeSeries = (1 to n).map { _ =>
      current = 0.8 * current + 0.2 * util.Random.nextGaussian()
      current
    }.toList
    
    val blockErrors = AdvancedStatistics.blockingAnalysis(timeSeries, 50)
    
    // Error should plateau at optimal block size
    val errors = blockErrors.toList.sortBy(_._1).map(_._2)
    val plateau = errors.drop(10).take(20)
    
    // Check that errors stabilize (relative variation < 50%)
    val maxError = plateau.max
    val minError = plateau.min
    if (maxError > 0) {
      (maxError - minError) / maxError should be < 0.5
    }
  }
}