package analysis

import scala.util.Random
import scala.math._

/**
 * Results from bootstrap analysis
 */
case class BootstrapResults(
  estimate: Double,
  standardError: Double,
  confidenceInterval: (Double, Double),
  bias: Double,
  samples: List[Double]
)

/**
 * Results from jackknife analysis
 */
case class JackknifeResults(
  estimate: Double,
  standardError: Double,
  bias: Double,
  values: List[Double]
)

/**
 * Results from maximum likelihood estimation
 */
case class MLEResults(
  estimate: Double,
  standardError: Double,
  logLikelihood: Double,
  converged: Boolean,
  iterations: Int
)

/**
 * Advanced statistical methods for error estimation and analysis
 */
object AdvancedStatistics {
  
  /**
   * Bootstrap method for error estimation
   * Resamples data with replacement to estimate uncertainty
   */
  def bootstrap[A](
    data: List[A],
    statistic: List[A] => Double,
    nSamples: Int = 1000,
    confidenceLevel: Double = 0.95
  ): BootstrapResults = {
    if (data.isEmpty) {
      return BootstrapResults(0.0, 0.0, (0.0, 0.0), 0.0, List.empty)
    }
    
    val random = new Random(42) // Fixed seed for reproducibility
    val n = data.length
    
    // Generate bootstrap samples
    val bootstrapEstimates = (1 to nSamples).map { _ =>
      val resample = List.fill(n)(data(random.nextInt(n)))
      statistic(resample)
    }.toList
    
    // Calculate statistics
    val originalEstimate = statistic(data)
    val mean = bootstrapEstimates.sum / nSamples
    val variance = bootstrapEstimates.map(x => pow(x - mean, 2)).sum / (nSamples - 1)
    val standardError = sqrt(variance)
    
    // Bias estimation
    val bias = mean - originalEstimate
    
    // Confidence interval (percentile method)
    val sorted = bootstrapEstimates.sorted
    val alpha = 1.0 - confidenceLevel
    val lowerIndex = (alpha / 2 * nSamples).toInt
    val upperIndex = ((1 - alpha / 2) * nSamples).toInt
    val confidenceInterval = (sorted(lowerIndex), sorted(upperIndex))
    
    BootstrapResults(
      estimate = originalEstimate,
      standardError = standardError,
      confidenceInterval = confidenceInterval,
      bias = bias,
      samples = bootstrapEstimates
    )
  }
  
  /**
   * Jackknife method for bias and variance estimation
   * Systematically leaves out one observation at a time
   */
  def jackknife[A](
    data: List[A],
    statistic: List[A] => Double
  ): JackknifeResults = {
    if (data.isEmpty) {
      return JackknifeResults(0.0, 0.0, 0.0, List.empty)
    }
    
    val n = data.length
    val fullEstimate = statistic(data)
    
    // Calculate leave-one-out estimates
    val jackknifeSamples = data.indices.map { i =>
      val leaveOneOut = data.take(i) ++ data.drop(i + 1)
      statistic(leaveOneOut)
    }.toList
    
    // Pseudovalues
    val pseudovalues = jackknifeSamples.map { theta_i =>
      n * fullEstimate - (n - 1) * theta_i
    }
    
    // Jackknife estimate (mean of pseudovalues)
    val jackknifeEstimate = pseudovalues.sum / n
    
    // Variance estimation
    val variance = pseudovalues.map { p =>
      pow(p - jackknifeEstimate, 2)
    }.sum / (n * (n - 1))
    
    val standardError = sqrt(variance)
    
    // Bias estimation
    val bias = (n - 1) * (jackknifeSamples.sum / n - fullEstimate)
    
    JackknifeResults(
      estimate = jackknifeEstimate,
      standardError = standardError,
      bias = bias,
      values = pseudovalues
    )
  }
  
  /**
   * Maximum likelihood estimation for power-law exponent
   * Uses Clauset et al. (2009) method
   */
  def maximumLikelihoodExponent(
    powerLawData: List[Double],
    xMin: Double
  ): MLEResults = {
    val filteredData = powerLawData.filter(_ >= xMin)
    
    if (filteredData.isEmpty) {
      return MLEResults(0.0, 0.0, Double.NegativeInfinity, false, 0)
    }
    
    val n = filteredData.length
    
    // MLE for power-law exponent: α = 1 + n / Σ ln(x_i / x_min)
    val sumLog = filteredData.map(x => log(x / xMin)).sum
    val alpha = 1.0 + n / sumLog
    
    // Standard error estimation
    val standardError = (alpha - 1) / sqrt(n)
    
    // Log-likelihood
    val logLikelihood = n * log(alpha - 1) - n * log(xMin) - alpha * sumLog
    
    // Check convergence (simplified - always converges for power law)
    val converged = alpha > 1.0 && alpha < 10.0
    
    MLEResults(
      estimate = alpha,
      standardError = standardError,
      logLikelihood = logLikelihood,
      converged = converged,
      iterations = 1
    )
  }
  
  /**
   * Kolmogorov-Smirnov test for goodness of fit
   */
  def kolmogorovSmirnovTest(
    data: List[Double],
    cdf: Double => Double
  ): (Double, Double) = { // Returns (D statistic, p-value)
    val sorted = data.sorted
    val n = data.length
    
    // Calculate maximum distance between empirical and theoretical CDF
    val distances = sorted.zipWithIndex.map { case (x, i) =>
      val empiricalCDF = (i + 1).toDouble / n
      val theoreticalCDF = cdf(x)
      abs(empiricalCDF - theoreticalCDF)
    }
    
    val dStatistic = distances.max
    
    // Approximate p-value (Marsaglia et al. approximation)
    val sqrtN = sqrt(n)
    val lambda = (sqrtN + 0.12 + 0.11 / sqrtN) * dStatistic
    val pValue = 2 * exp(-2 * lambda * lambda)
    
    (dStatistic, pValue)
  }
  
  /**
   * Weighted least squares for fitting with errors
   */
  def weightedLeastSquares(
    data: List[(Double, Double, Double)] // (x, y, error)
  ): (Double, Double, Double) = { // Returns (slope, intercept, chi-squared)
    if (data.length < 2) return (0.0, 0.0, Double.PositiveInfinity)
    
    // Weights are 1/error²
    val weights = data.map { case (_, _, err) => 
      if (err > 0) 1.0 / (err * err) else 1.0
    }
    
    val sumW = weights.sum
    val sumWX = data.zip(weights).map { case ((x, _, _), w) => w * x }.sum
    val sumWY = data.zip(weights).map { case ((_, y, _), w) => w * y }.sum
    val sumWXY = data.zip(weights).map { case ((x, y, _), w) => w * x * y }.sum
    val sumWX2 = data.zip(weights).map { case ((x, _, _), w) => w * x * x }.sum
    
    val delta = sumW * sumWX2 - sumWX * sumWX
    val slope = (sumW * sumWXY - sumWX * sumWY) / delta
    val intercept = (sumWX2 * sumWY - sumWX * sumWXY) / delta
    
    // Calculate chi-squared
    val chiSquared = data.zip(weights).map { case ((x, y, _), w) =>
      val predicted = slope * x + intercept
      w * pow(y - predicted, 2)
    }.sum
    
    (slope, intercept, chiSquared)
  }
  
  /**
   * Bayesian information criterion for model selection
   */
  def bayesianInformationCriterion(
    logLikelihood: Double,
    nParameters: Int,
    nDataPoints: Int
  ): Double = {
    -2 * logLikelihood + nParameters * log(nDataPoints)
  }
  
  /**
   * Akaike information criterion for model selection
   */
  def akaikeInformationCriterion(
    logLikelihood: Double,
    nParameters: Int
  ): Double = {
    2 * nParameters - 2 * logLikelihood
  }
  
  /**
   * Cross-validation for model validation
   */
  def crossValidation[A](
    data: List[A],
    nFolds: Int,
    trainAndEvaluate: (List[A], List[A]) => Double
  ): (Double, Double) = { // Returns (mean score, std error)
    if (data.length < nFolds) return (0.0, 0.0)
    
    val foldSize = data.length / nFolds
    val shuffled = Random.shuffle(data)
    
    val scores = (0 until nFolds).map { i =>
      val testStart = i * foldSize
      val testEnd = if (i == nFolds - 1) data.length else (i + 1) * foldSize
      
      val testSet = shuffled.slice(testStart, testEnd)
      val trainSet = shuffled.take(testStart) ++ shuffled.drop(testEnd)
      
      trainAndEvaluate(trainSet, testSet)
    }.toList
    
    val meanScore = scores.sum / scores.length
    val variance = scores.map(s => pow(s - meanScore, 2)).sum / (scores.length - 1)
    val stdError = sqrt(variance) / sqrt(scores.length)
    
    (meanScore, stdError)
  }
  
  /**
   * Autocorrelation-corrected error estimation
   */
  def autocorrelationCorrectedError(
    timeSeries: List[Double],
    autocorrelation: Map[Int, Double]
  ): Double = {
    val n = timeSeries.length
    val mean = timeSeries.sum / n
    val variance = timeSeries.map(x => pow(x - mean, 2)).sum / n
    
    // Integrated autocorrelation time
    val tau = 1.0 + 2.0 * autocorrelation.filter(_._1 > 0).values.sum
    
    // Effective sample size
    val nEffective = n / tau
    
    // Corrected standard error
    sqrt(variance / nEffective)
  }
  
  /**
   * Blocking method for correlated data
   */
  def blockingAnalysis(
    timeSeries: List[Double],
    maxBlockSize: Int = 100
  ): Map[Int, Double] = { // Returns block size -> error estimate
    val n = timeSeries.length
    val mean = timeSeries.sum / n
    
    (1 to min(maxBlockSize, n / 2)).map { blockSize =>
      val nBlocks = n / blockSize
      val blockMeans = (0 until nBlocks).map { i =>
        val block = timeSeries.slice(i * blockSize, (i + 1) * blockSize)
        block.sum / block.length
      }.toList
      
      val blockVariance = blockMeans.map(m => pow(m - mean, 2)).sum / (nBlocks - 1)
      val error = sqrt(blockVariance / nBlocks)
      
      blockSize -> error
    }.toMap
  }
}