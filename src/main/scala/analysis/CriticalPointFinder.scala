package analysis

import cats.effect._
import cats.implicits._
import models._
import simulation._

/**
 * Represents a critical point in parameter space
 */
case class CriticalPoint(
  parameter: PhaseParameter,
  value: Double,
  confidence: Double,
  method: String,
  orderParameter: String
)

/**
 * Methods for finding critical points in phase transitions
 */
object CriticalPointFinder {
  
  /**
   * Find critical point using bisection search
   * Suitable when order parameter changes monotonically
   */
  def bisectionSearch[F[_]: Sync](
    parameter: PhaseParameter,
    range: (Double, Double),
    tolerance: Double,
    criterion: Map[String, Double] => Boolean,
    runner: SimulationRunner[F],
    baseState: SimulationState,
    config: AnalysisConfig
  ): F[Option[CriticalPoint]] = {
    
    def search(low: Double, high: Double, depth: Int): F[Option[CriticalPoint]] = {
      if (depth > 20 || (high - low) < tolerance) {
        // Reached maximum depth or tolerance
        val mid = (low + high) / 2.0
        Sync[F].pure(Some(CriticalPoint(
          parameter = parameter,
          value = mid,
          confidence = 1.0 - (high - low) / (range._2 - range._1),
          method = "bisection",
          orderParameter = "burntFraction"
        )))
      } else {
        val mid = (low + high) / 2.0
        
        // Run simulation at midpoint
        val modifiedState = parameter.applyToState(baseState, mid)
        runner.runEnsemble(modifiedState, config).flatMap { results =>
          val orderParams = results.averageOrderParameters
          
          if (criterion(orderParams)) {
            // Critical point is in lower half
            search(low, mid, depth + 1)
          } else {
            // Critical point is in upper half
            search(mid, high, depth + 1)
          }
        }
      }
    }
    
    search(range._1, range._2, 0)
  }
  
  /**
   * Find critical point from susceptibility peak
   * Variance of order parameter peaks at criticality
   */
  def susceptibilityPeak(
    data: List[ParameterPoint],
    orderParameterName: String = "burntFraction"
  ): Option[CriticalPoint] = {
    if (data.length < 3) return None
    
    // Calculate susceptibility (variance) for each point
    val susceptibilities = data.map { point =>
      val variance = point.ensemble.standardDeviations.getOrElse(orderParameterName, 0.0)
      val systemSize = 100.0 // Approximate system size
      (point.value, variance * variance * systemSize)
    }
    
    // Find maximum
    val maxPoint = susceptibilities.maxBy(_._2)
    val maxIndex = susceptibilities.indexWhere(_._2 == maxPoint._2)
    
    // Estimate confidence based on peak prominence
    val prominence = if (susceptibilities.length > 1) {
      val others = susceptibilities.filter(_ != maxPoint)
      val avgOther = others.map(_._2).sum / others.length
      (maxPoint._2 - avgOther) / maxPoint._2
    } else 0.0
    
    Some(CriticalPoint(
      parameter = data.head.parameter,
      value = maxPoint._1,
      confidence = prominence.min(1.0).max(0.0),
      method = "susceptibility_peak",
      orderParameter = orderParameterName
    ))
  }
  
  /**
   * Find critical point using Binder cumulant crossing
   * The fourth-order Binder cumulant is size-independent at criticality
   */
  def binderCumulant(
    dataBySizeMap: Map[Int, List[ParameterPoint]],
    orderParameterName: String = "burntFraction"
  ): Option[CriticalPoint] = {
    if (dataBySizeMap.size < 2) return None
    
    val sizes = dataBySizeMap.keys.toList.sorted
    val parameterValues = dataBySizeMap.values.headOption.map(_.map(_.value)).getOrElse(List.empty)
    
    if (parameterValues.isEmpty) return None
    
    // Calculate Binder cumulant for each size and parameter value
    val binderCurves = dataBySizeMap.map { case (size, points) =>
      val cumulants = points.map { point =>
        val values = point.ensemble.results.map(_.orderParameters.getOrElse(orderParameterName, 0.0))
        val binder = calculateBinderCumulant(values)
        (point.value, binder)
      }
      (size, cumulants)
    }
    
    // Find crossing points between different sizes
    val crossingPoints = for {
      i <- 0 until sizes.length - 1
      j <- i + 1 until sizes.length
      size1 = sizes(i)
      size2 = sizes(j)
      curve1 = binderCurves(size1)
      curve2 = binderCurves(size2)
      crossing <- findCrossing(curve1, curve2)
    } yield crossing
    
    if (crossingPoints.isEmpty) return None
    
    // Average crossing points
    val avgCrossing = crossingPoints.sum / crossingPoints.length
    val stdDev = math.sqrt(
      crossingPoints.map(x => math.pow(x - avgCrossing, 2)).sum / crossingPoints.length
    )
    
    Some(CriticalPoint(
      parameter = dataBySizeMap.values.head.head.parameter,
      value = avgCrossing,
      confidence = 1.0 - stdDev / avgCrossing,
      method = "binder_cumulant",
      orderParameter = orderParameterName
    ))
  }
  
  /**
   * Find critical point from finite-size scaling collapse
   */
  def finiteScalingCollapse(
    dataBySizeMap: Map[Int, List[ParameterPoint]],
    criticalExponents: CriticalExponents,
    orderParameterName: String = "burntFraction"
  ): Option[CriticalPoint] = {
    // This is a more advanced method requiring optimization
    // For now, return a simple estimate
    susceptibilityPeak(
      dataBySizeMap.values.flatten.toList,
      orderParameterName
    )
  }
  
  /**
   * Estimate critical exponents near the critical point
   */
  def estimateCriticalExponents(
    data: List[ParameterPoint],
    criticalValue: Double,
    orderParameterName: String = "burntFraction"
  ): CriticalExponents = {
    // Filter data near critical point
    val nearCritical = data.filter(p => 
      math.abs(p.value - criticalValue) < 0.1 && p.value != criticalValue
    )
    
    if (nearCritical.length < 2) {
      return CriticalExponents() // Default values
    }
    
    // Estimate beta exponent: order parameter ~ |p - pc|^beta
    val betaData = nearCritical.map { point =>
      val orderParam = point.ensemble.averageOrderParameters.getOrElse(orderParameterName, 0.0)
      val distance = math.abs(point.value - criticalValue)
      (math.log(distance), math.log(orderParam + 0.001))
    }.filter(_._2.isFinite)
    
    val beta = if (betaData.nonEmpty) {
      estimateExponent(betaData)
    } else 0.5
    
    // Estimate other exponents similarly
    CriticalExponents(
      beta = beta,
      gamma = 1.0,  // Would need susceptibility data
      nu = 1.0,     // Would need correlation length data
      alpha = 0.0,  // From scaling relation
      delta = 3.0,  // Would need field dependence
      eta = 0.25    // Would need correlation function
    )
  }
  
  // Helper methods
  
  private def calculateBinderCumulant(values: List[Double]): Double = {
    val n = values.length
    if (n == 0) return 0.0
    
    val mean = values.sum / n
    val m2 = values.map(x => math.pow(x - mean, 2)).sum / n
    val m4 = values.map(x => math.pow(x - mean, 4)).sum / n
    
    if (m2 > 0) 1.0 - m4 / (3.0 * m2 * m2) else 0.0
  }
  
  private def findCrossing(curve1: List[(Double, Double)], curve2: List[(Double, Double)]): Option[Double] = {
    // Simple linear interpolation to find crossing
    for (i <- 0 until curve1.length - 1) {
      val (x1, y1) = curve1(i)
      val (x2, y2) = curve1(i + 1)
      
      // Find corresponding points in curve2
      curve2.find(_._1 == x1).flatMap { case (_, y1_2) =>
        curve2.find(_._1 == x2).map { case (_, y2_2) =>
          // Check if curves cross in this interval
          val diff1 = y1 - y1_2
          val diff2 = y2 - y2_2
          
          if (diff1 * diff2 < 0) {
            // Curves cross - linear interpolation
            x1 + (x2 - x1) * math.abs(diff1) / (math.abs(diff1) + math.abs(diff2))
          } else {
            -1.0 // No crossing
          }
        }
      }.filter(_ > 0)
    }
    None
  }
  
  private def estimateExponent(logData: List[(Double, Double)]): Double = {
    // Simple linear regression in log-log space
    val n = logData.length
    val sumX = logData.map(_._1).sum
    val sumY = logData.map(_._2).sum
    val sumXY = logData.map(p => p._1 * p._2).sum
    val sumX2 = logData.map(p => p._1 * p._1).sum
    
    (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
  }
}

/**
 * Critical exponents characterizing the universality class
 */
case class CriticalExponents(
  beta: Double = 0.5,    // Order parameter exponent
  gamma: Double = 1.0,   // Susceptibility exponent
  nu: Double = 1.0,      // Correlation length exponent
  alpha: Double = 0.0,   // Specific heat exponent
  delta: Double = 3.0,   // Critical isotherm exponent
  eta: Double = 0.25     // Correlation function exponent
) {
  // Scaling relations
  def checkScalingRelations(): Boolean = {
    val alphRelation = math.abs(alpha + 2 * beta + gamma - 2)
    val rushbrookeRelation = math.abs(alpha + 2 * beta + gamma - 2)
    val fisherRelation = math.abs(gamma - beta * (delta - 1))
    
    alphRelation < 0.1 && rushbrookeRelation < 0.1 && fisherRelation < 0.1
  }
}