package analysis

import models._
import simulation._
import scala.math._

/**
 * Report on finite-size effects in the data
 */
case class FiniteSizeReport(
  affected: Boolean,
  largestSafeSize: Option[Int],
  correlationLengthRatio: Map[Int, Double],
  recommendations: List[String]
)

/**
 * Report on system equilibration
 */
case class EquilibrationReport(
  equilibrated: Boolean,
  equilibrationTime: Double,
  driftDetected: Boolean,
  autocorrelationTime: Double,
  recommendations: List[String]
)

/**
 * Report on statistical significance
 */
case class SignificanceReport(
  significant: Boolean,
  pValues: Map[String, Double],
  effectSizes: Map[String, Double],
  sampleSizeAdequate: Boolean,
  powerAnalysis: Map[String, Double]
)

/**
 * Overall data quality assessment
 */
case class DataQualityAssessment(
  overallQuality: String, // "excellent", "good", "fair", "poor"
  finiteSizeReport: FiniteSizeReport,
  equilibrationReport: EquilibrationReport,
  significanceReport: SignificanceReport,
  warnings: List[String],
  recommendations: List[String]
)

/**
 * Data quality analysis and validation
 */
object DataQualityAnalysis {
  
  /**
   * Check for finite-size effects
   */
  def finiteSizeEffects(
    data: Map[Int, List[ParameterPoint]]
  ): FiniteSizeReport = {
    if (data.size < 2) {
      return FiniteSizeReport(
        affected = true,
        largestSafeSize = None,
        correlationLengthRatio = Map.empty,
        recommendations = List("Need at least 2 system sizes for finite-size analysis")
      )
    }
    
    val sizes = data.keys.toList.sorted
    
    // Calculate correlation length for each size
    val correlationLengths = data.map { case (size, points) =>
      val criticalPoint = points.find(p => 
        p.ensemble.phase == Critical
      ).getOrElse(points(points.length / 2))
      
      val correlations = SpatialCorrelations.twoPointCorrelation(
        criticalPoint.ensemble.results.head.finalState,
        size / 4
      )
      val xi = SpatialCorrelations.correlationLength(correlations)
      
      size -> (xi / size)
    }
    
    // Check if ξ/L is small enough (< 0.1 is good)
    val safeSizes = sizes.filter(L => correlationLengths(L) < 0.1)
    val safeSize = if (safeSizes.nonEmpty) Some(safeSizes.max) else None
    val affected = correlationLengths.values.exists(_ > 0.2)
    
    val recommendations = if (affected) {
      List(
        "Finite-size effects detected",
        s"Use system sizes larger than ${safeSize.getOrElse(sizes.max * 2)}",
        "Consider finite-size scaling analysis"
      )
    } else {
      List("Finite-size effects minimal")
    }
    
    FiniteSizeReport(
      affected = affected,
      largestSafeSize = safeSize,
      correlationLengthRatio = correlationLengths,
      recommendations = recommendations
    )
  }
  
  /**
   * Verify equilibration of time series
   */
  def checkEquilibration(
    timeSeries: List[SimulationState]
  ): EquilibrationReport = {
    if (timeSeries.length < 10) {
      return EquilibrationReport(
        equilibrated = false,
        equilibrationTime = 0.0,
        driftDetected = true,
        autocorrelationTime = 0.0,
        recommendations = List("Time series too short for equilibration analysis")
      )
    }
    
    // Extract order parameter time series
    val orderParams = timeSeries.map(state => 
      OrderParameters.burntFraction(state)
    )
    
    // Check for drift using linear regression
    val times = timeSeries.indices.map(_.toDouble).toList
    val (slope, _, r2) = linearFit(times.zip(orderParams))
    val driftDetected = abs(slope) > 0.001 && r2 > 0.5
    
    // Find equilibration time using reverse cumulative average
    val equilibrationIndex = findEquilibrationPoint(orderParams)
    val equilibrationTime = timeSeries(equilibrationIndex).elapsedTime
    
    // Calculate autocorrelation time
    val equilibratedData = orderParams.drop(equilibrationIndex)
    val autocorr = TemporalCorrelations.autocorrelation(
      equilibratedData.map(v => Map("value" -> v)),
      "value",
      min(100, equilibratedData.length / 4)
    )
    val autocorrelationTime = TemporalCorrelations.relaxationTime(autocorr)
    
    // Check if we have enough independent samples
    val independentSamples = equilibratedData.length / autocorrelationTime
    val equilibrated = independentSamples > 20 && !driftDetected
    
    val recommendations = List.newBuilder[String]
    if (driftDetected) recommendations += "System shows drift - not equilibrated"
    if (independentSamples < 20) recommendations += s"Need longer runs: only ${independentSamples.toInt} independent samples"
    if (equilibrationTime > timeSeries.last.elapsedTime * 0.5) {
      recommendations += "Equilibration takes more than half the run time - consider longer warmup"
    }
    
    EquilibrationReport(
      equilibrated = equilibrated,
      equilibrationTime = equilibrationTime,
      driftDetected = driftDetected,
      autocorrelationTime = autocorrelationTime,
      recommendations = recommendations.result()
    )
  }
  
  /**
   * Assess statistical significance of results
   */
  def statisticalSignificance(
    results: StatisticalMechanicsResults,
    nullHypothesis: CriticalExponents = MeanField.expectedExponents
  ): SignificanceReport = {
    // Calculate p-values for exponent differences
    val pValues = Map(
      "beta" -> calculatePValue(
        results.criticalExponents.beta,
        results.exponentErrors.getOrElse("beta", 0.1),
        nullHypothesis.beta
      ),
      "gamma" -> calculatePValue(
        results.criticalExponents.gamma,
        results.exponentErrors.getOrElse("gamma", 0.1),
        nullHypothesis.gamma
      ),
      "nu" -> calculatePValue(
        results.criticalExponents.nu,
        results.exponentErrors.getOrElse("nu", 0.1),
        nullHypothesis.nu
      )
    )
    
    // Calculate effect sizes (Cohen's d)
    val effectSizes = Map(
      "beta" -> abs(results.criticalExponents.beta - nullHypothesis.beta) / 
                 results.exponentErrors.getOrElse("beta", 0.1),
      "gamma" -> abs(results.criticalExponents.gamma - nullHypothesis.gamma) / 
                  results.exponentErrors.getOrElse("gamma", 0.1),
      "nu" -> abs(results.criticalExponents.nu - nullHypothesis.nu) / 
               results.exponentErrors.getOrElse("nu", 0.1)
    )
    
    // Power analysis (simplified)
    val powerAnalysis = effectSizes.map { case (exp, effect) =>
      exp -> calculatePower(effect, 100) // Assuming 100 samples
    }
    
    val significant = pValues.values.exists(_ < 0.05)
    val sampleSizeAdequate = powerAnalysis.values.forall(_ > 0.8)
    
    SignificanceReport(
      significant = significant,
      pValues = pValues,
      effectSizes = effectSizes,
      sampleSizeAdequate = sampleSizeAdequate,
      powerAnalysis = powerAnalysis
    )
  }
  
  /**
   * Comprehensive data quality assessment
   */
  def assessDataQuality(
    data: Map[Int, List[ParameterPoint]],
    timeSeries: List[SimulationState],
    results: StatisticalMechanicsResults
  ): DataQualityAssessment = {
    val finiteSizeReport = finiteSizeEffects(data)
    val equilibrationReport = checkEquilibration(timeSeries)
    val significanceReport = statisticalSignificance(results)
    
    val warnings = List.newBuilder[String]
    val recommendations = List.newBuilder[String]
    
    // Collect warnings
    if (finiteSizeReport.affected) warnings += "Finite-size effects present"
    if (!equilibrationReport.equilibrated) warnings += "System not fully equilibrated"
    if (!significanceReport.sampleSizeAdequate) warnings += "Sample size may be inadequate"
    if (results.hyperscalingViolation > 0.1) warnings += "Hyperscaling relations violated"
    
    // Overall quality assessment
    val qualityScore = List(
      if (finiteSizeReport.affected) 0 else 1,
      if (equilibrationReport.equilibrated) 1 else 0,
      if (significanceReport.significant) 1 else 0,
      if (significanceReport.sampleSizeAdequate) 1 else 0,
      if (results.scalingQuality > 0.9) 1 else 0
    ).sum
    
    val overallQuality = qualityScore match {
      case 5 => "excellent"
      case 4 => "good"
      case 3 => "fair"
      case _ => "poor"
    }
    
    // Collect recommendations
    recommendations ++= finiteSizeReport.recommendations
    recommendations ++= equilibrationReport.recommendations
    
    if (!significanceReport.sampleSizeAdequate) {
      recommendations += "Increase ensemble size for better statistics"
    }
    
    if (results.scalingQuality < 0.9) {
      recommendations += "Data collapse quality is poor - check for corrections to scaling"
    }
    
    DataQualityAssessment(
      overallQuality = overallQuality,
      finiteSizeReport = finiteSizeReport,
      equilibrationReport = equilibrationReport,
      significanceReport = significanceReport,
      warnings = warnings.result(),
      recommendations = recommendations.result().distinct
    )
  }
  
  /**
   * Check for systematic errors
   */
  def checkSystematicErrors(
    ensembleResults: List[EnsembleResults]
  ): Map[String, Double] = {
    // Check for various systematic errors
    Map(
      "ensemble_size_bias" -> checkEnsembleSizeBias(ensembleResults),
      "boundary_effects" -> checkBoundaryEffects(ensembleResults),
      "discretization_error" -> checkDiscretizationError(ensembleResults)
    )
  }
  
  // Helper methods
  
  private def findEquilibrationPoint(data: List[Double]): Int = {
    if (data.length < 10) return 0
    
    // Use reverse cumulative average method
    val reverseMeans = data.reverse.scanLeft(0.0)(_ + _).tail
      .zip(1 to data.length)
      .map { case (sum, n) => sum / n }
      .reverse
    
    val totalMean = data.sum / data.length
    val tolerance = 0.05 * abs(totalMean)
    
    // Find where reverse mean stabilizes
    reverseMeans.zipWithIndex.find { case (mean, _) =>
      abs(mean - totalMean) < tolerance
    }.map(_._2).getOrElse(data.length / 2)
  }
  
  private def linearFit(data: List[(Double, Double)]): (Double, Double, Double) = {
    val n = data.length
    if (n < 2) return (0.0, 0.0, 0.0)
    
    val sumX = data.map(_._1).sum
    val sumY = data.map(_._2).sum
    val sumXY = data.map(p => p._1 * p._2).sum
    val sumX2 = data.map(p => p._1 * p._1).sum
    
    val slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
    val intercept = (sumY - slope * sumX) / n
    
    // R-squared
    val meanY = sumY / n
    val ssTotal = data.map(p => pow(p._2 - meanY, 2)).sum
    val ssResidual = data.map { case (x, y) =>
      val predicted = slope * x + intercept
      pow(y - predicted, 2)
    }.sum
    
    val r2 = if (ssTotal > 0) 1.0 - ssResidual / ssTotal else 0.0
    
    (slope, intercept, r2)
  }
  
  private def calculatePValue(
    observed: Double,
    standardError: Double,
    expected: Double
  ): Double = {
    val z = abs(observed - expected) / standardError
    2 * (1 - normalCDF(z))
  }
  
  private def normalCDF(z: Double): Double = {
    0.5 * (1 + erf(z / sqrt(2)))
  }
  
  private def erf(x: Double): Double = {
    // Approximation of error function
    val a1 =  0.254829592
    val a2 = -0.284496736
    val a3 =  1.421413741
    val a4 = -1.453152027
    val a5 =  1.061405429
    val p  =  0.3275911
    
    val sign = if (x < 0) -1 else 1
    val absX = abs(x)
    
    val t = 1.0 / (1.0 + p * absX)
    val y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * exp(-absX * absX)
    
    sign * y
  }
  
  private def calculatePower(effectSize: Double, sampleSize: Int): Double = {
    // Simplified power calculation
    val criticalZ = 1.96 // For alpha = 0.05
    val nonCentrality = effectSize * sqrt(sampleSize)
    normalCDF(nonCentrality - criticalZ) + normalCDF(-nonCentrality - criticalZ)
  }
  
  private def checkEnsembleSizeBias(ensembles: List[EnsembleResults]): Double = {
    // Check if results depend on ensemble size
    if (ensembles.length < 2) return 0.0
    
    val sizes = ensembles.map(_.results.length)
    val values = ensembles.map(_.averageOrderParameters.getOrElse("burntFraction", 0.0))
    
    val (slope, _, r2) = linearFit(sizes.map(_.toDouble).zip(values))
    abs(slope) * r2 // Bias metric
  }
  
  private def checkBoundaryEffects(ensembles: List[EnsembleResults]): Double = {
    // Simplified check - would need spatial analysis
    0.05 // Placeholder
  }
  
  private def checkDiscretizationError(ensembles: List[EnsembleResults]): Double = {
    // Check time step convergence - would need multiple dt runs
    0.01 // Placeholder
  }
}