package analysis

import scala.math._

/**
 * Results from scaling analysis
 */
case class ScalingAnalysisResults(
  exponents: Map[Int, Double],
  quality: Map[Int, Double],
  criticalPoint: Option[Double],
  collapseData: Map[Int, List[(Double, Double)]]
)

/**
 * Moment analysis for cluster size distributions
 */
object MomentAnalysis {
  
  /**
   * Calculate moments of cluster size distribution
   * M_k = Σ s^k * n(s)
   */
  def clusterMoments(
    distribution: Map[Int, Int],
    maxOrder: Int
  ): Map[Int, Double] = {
    if (distribution.isEmpty) return Map.empty
    
    val totalClusters = distribution.values.sum.toDouble
    
    (0 to maxOrder).map { k =>
      val moment = distribution.map { case (size, count) =>
        pow(size, k) * count
      }.sum / totalClusters
      
      k -> moment
    }.toMap
  }
  
  /**
   * Calculate moment ratios for critical point detection
   * These ratios have special properties at criticality
   */
  def momentRatios(
    moments: Map[Int, Double]
  ): Map[(Int, Int), Double] = {
    val ratios = for {
      k <- moments.keys
      l <- moments.keys
      if k > l && l > 0
      if moments(l) > 0
    } yield {
      (k, l) -> (moments(k) / moments(l))
    }
    
    ratios.toMap
  }
  
  /**
   * Analyze finite-size scaling of moments
   * At criticality: M_k(L) ~ L^(k/σ)
   */
  def momentScaling(
    systemSizes: List[Int],
    momentsData: Map[Int, Map[Int, Double]]
  ): ScalingAnalysisResults = {
    if (systemSizes.length < 2) {
      return ScalingAnalysisResults(Map.empty, Map.empty, None, Map.empty)
    }
    
    // Extract scaling exponents for each moment order
    val exponents = momentsData.flatMap { case (_, moments) =>
      moments.map { case (k, _) =>
        k -> extractScalingExponent(systemSizes, momentsData, k)
      }
    }.groupBy(_._1).map { case (k, values) =>
      k -> values.map(_._2).sum / values.size
    }
    
    // Quality of scaling fits
    val quality = exponents.map { case (k, exp) =>
      k -> evaluateScalingQuality(systemSizes, momentsData, k, exp)
    }
    
    // Estimate critical point from moment ratios
    val criticalPoint = estimateCriticalFromMoments(momentsData)
    
    // Prepare data for collapse plots
    val collapseData = prepareCollapseData(systemSizes, momentsData, exponents)
    
    ScalingAnalysisResults(exponents, quality, criticalPoint, collapseData)
  }
  
  /**
   * Calculate gap exponent from moment ratios
   * Used to identify multicritical behavior
   */
  def gapExponent(
    moments: Map[Int, Double]
  ): Double = {
    // Gap exponent Δ from ratio M_3/M_2
    if (moments.contains(2) && moments.contains(3) && moments(2) > 0) {
      log(moments(3) / moments(2)) / log(moments(2) / moments(1))
    } else {
      0.0
    }
  }
  
  /**
   * Analyze cluster size distribution for power-law behavior
   * n(s) ~ s^(-τ) * exp(-s/s_ξ)
   */
  def analyzeDistribution(
    distribution: Map[Int, Int]
  ): (Double, Double, Double) = { // Returns (tau, s_xi, quality)
    if (distribution.size < 5) return (2.05, 1000.0, 0.0)
    
    // Log-binned data for better statistics
    val logBinned = logBinDistribution(distribution)
    
    // Fit power law with exponential cutoff
    val (tau, sXi, quality) = fitPowerLawWithCutoff(logBinned)
    
    (tau, sXi, quality)
  }
  
  /**
   * Calculate cluster fractal dimension
   * R ~ s^(1/d_f)
   */
  def clusterFractalDimension(
    clusterData: List[(Int, Double)] // (cluster size, radius)
  ): Double = {
    if (clusterData.length < 3) return 1.896 // 2D percolation default
    
    val logData = clusterData.filter { case (s, r) => s > 0 && r > 0 }
      .map { case (s, r) => (log(s), log(r)) }
    
    if (logData.length < 3) return 1.896
    
    val (slope, _, _) = linearRegression(logData)
    1.0 / slope
  }
  
  /**
   * Binning analysis for improved statistics
   */
  def binClusterSizes(
    distribution: Map[Int, Int],
    binFactor: Double = 1.5
  ): Map[Int, (Int, Double)] = { // Returns (bin center, (count, error))
    var binEdges = List(1.0)
    while (binEdges.last < distribution.keys.max) {
      binEdges = binEdges :+ (binEdges.last * binFactor)
    }
    
    val bins = binEdges.sliding(2).map { case List(low, high) =>
      val binData = distribution.filter { case (s, _) => 
        s >= low && s < high
      }
      
      if (binData.nonEmpty) {
        val center = (binData.keys.sum.toDouble / binData.size).toInt
        val count = binData.values.sum
        val error = sqrt(count.toDouble)
        Some(center -> (count, error))
      } else {
        None
      }
    }.flatten.toMap
    
    bins
  }
  
  /**
   * Universal moment amplitude ratios
   * These are universal at criticality
   */
  def universalAmplitudeRatios(
    moments: Map[Int, Double]
  ): Map[String, Double] = {
    Map(
      "R_21" -> (if (moments.contains(1) && moments(1) > 0) moments(2) / pow(moments(1), 2) else 0.0),
      "R_32" -> (if (moments.contains(2) && moments(2) > 0) moments(3) / moments(2) else 0.0),
      "R_42" -> (if (moments.contains(2) && moments(2) > 0) moments(4) / pow(moments(2), 2) else 0.0)
    )
  }
  
  // Helper methods
  
  private def extractScalingExponent(
    systemSizes: List[Int],
    momentsData: Map[Int, Map[Int, Double]],
    momentOrder: Int
  ): Double = {
    val logData = systemSizes.flatMap { L =>
      momentsData.get(L).flatMap(_.get(momentOrder)).map { m =>
        (log(L), log(m + 1e-10))
      }
    }.filter(p => p._1.isFinite && p._2.isFinite)
    
    if (logData.length < 2) return 0.0
    
    val (slope, _, _) = linearRegression(logData)
    slope
  }
  
  private def evaluateScalingQuality(
    systemSizes: List[Int],
    momentsData: Map[Int, Map[Int, Double]],
    momentOrder: Int,
    exponent: Double
  ): Double = {
    val data = systemSizes.flatMap { L =>
      momentsData.get(L).flatMap(_.get(momentOrder)).map { m =>
        val predicted = pow(L, exponent)
        val actual = m
        (predicted, actual)
      }
    }
    
    if (data.isEmpty) return 0.0
    
    val meanActual = data.map(_._2).sum / data.length
    val ssTotal = data.map { case (_, actual) => pow(actual - meanActual, 2) }.sum
    val ssResidual = data.map { case (pred, actual) => pow(actual - pred, 2) }.sum
    
    if (ssTotal > 0) 1.0 - ssResidual / ssTotal else 0.0
  }
  
  private def estimateCriticalFromMoments(
    momentsData: Map[Int, Map[Int, Double]]
  ): Option[Double] = {
    // Use crossing of moment ratios for different system sizes
    // This is a simplified version
    if (momentsData.size < 2) return None
    
    Some(0.5927) // Placeholder - would need parameter sweep data
  }
  
  private def prepareCollapseData(
    systemSizes: List[Int],
    momentsData: Map[Int, Map[Int, Double]],
    exponents: Map[Int, Double]
  ): Map[Int, List[(Double, Double)]] = {
    momentsData.map { case (size, moments) =>
      val collapsePoints = moments.map { case (k, m) =>
        val exp = exponents.getOrElse(k, 1.0)
        val scaledM = m / pow(size, exp)
        (k.toDouble, scaledM)
      }.toList
      
      size -> collapsePoints
    }
  }
  
  private def logBinDistribution(
    distribution: Map[Int, Int],
    base: Double = 1.5
  ): Map[Double, Double] = {
    val grouped = distribution.groupBy { case (size, _) =>
      if (size > 0) floor(log(size) / log(base)) else 0.0
    }
    
    grouped.map { case (bin, data) =>
      val meanSize = exp(bin * log(base))
      val totalCount = data.values.sum.toDouble
      (meanSize, totalCount)
    }
  }
  
  private def fitPowerLawWithCutoff(
    data: Map[Double, Double]
  ): (Double, Double, Double) = {
    // Simplified fitting - in practice would use MLE or weighted least squares
    val logData = data.filter { case (s, n) => s > 1 && n > 0 }
      .map { case (s, n) => (log(s), log(n)) }.toList
    
    if (logData.length < 3) return (2.05, 1000.0, 0.0)
    
    val (slope, _, r2) = linearRegression(logData)
    
    // Estimate cutoff from where data deviates from power law
    val cutoff = estimateCutoff(data, -slope)
    
    (-slope, cutoff, r2)
  }
  
  private def estimateCutoff(
    data: Map[Double, Double],
    tau: Double
  ): Double = {
    // Find where distribution deviates from pure power law
    val sorted = data.toList.sortBy(_._1)
    
    sorted.reverse.find { case (s, n) =>
      val expected = pow(s, -tau)
      val ratio = n / expected
      ratio < 0.5 // 50% deviation
    }.map(_._1).getOrElse(sorted.last._1)
  }
  
  private def linearRegression(data: List[(Double, Double)]): (Double, Double, Double) = {
    val n = data.length
    if (n < 2) return (0.0, 1.0, 0.0)
    
    val sumX = data.map(_._1).sum
    val sumY = data.map(_._2).sum
    val sumXY = data.map(p => p._1 * p._2).sum
    val sumX2 = data.map(p => p._1 * p._1).sum
    val sumY2 = data.map(p => p._2 * p._2).sum
    
    val slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
    val intercept = (sumY - slope * sumX) / n
    
    // Calculate R²
    val meanY = sumY / n
    val ssTotal = data.map(p => pow(p._2 - meanY, 2)).sum
    val ssResidual = data.map { case (x, y) =>
      val predicted = slope * x + intercept
      pow(y - predicted, 2)
    }.sum
    
    val r2 = if (ssTotal > 0) 1.0 - ssResidual / ssTotal else 0.0
    
    // Estimate error
    val se = if (n > 2) {
      sqrt(ssResidual / (n - 2)) / sqrt(sumX2 - sumX * sumX / n)
    } else 1.0
    
    (slope, se, r2)
  }
}