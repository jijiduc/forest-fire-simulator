package analysis

import models._
import simulation._

/**
 * Represents the phase of the system
 */
sealed trait Phase {
  def name: String
  def description: String
}

case object SubCritical extends Phase {
  val name = "Sub-critical"
  val description = "Small isolated fires that self-extinguish"
}

case object Critical extends Phase {
  val name = "Critical"
  val description = "Power-law distributed fire sizes at percolation threshold"
}

case object SuperCritical extends Phase {
  val name = "Super-critical"
  val description = "System-spanning fires that consume most of the forest"
}

/**
 * Identifies the phase of the system based on order parameters
 */
object PhaseIdentifier {
  
  /**
   * Identify phase from a single simulation state
   */
  def identifyPhase(state: SimulationState): Phase = {
    val burntFrac = OrderParameters.burntFraction(state)
    val largestCluster = OrderParameters.largestClusterRatio(state)
    val percolation = OrderParameters.percolationIndicator(state)
    
    identifyFromParameters(burntFrac, largestCluster, percolation)
  }
  
  /**
   * Identify phase from order parameters
   */
  def identifyFromParameters(
    burntFraction: Double,
    largestClusterRatio: Double,
    percolationIndicator: Double
  ): Phase = {
    // Super-critical: large burnt fraction and percolation
    if (burntFraction > 0.4 && percolationIndicator > 0.5) {
      SuperCritical
    }
    // Sub-critical: small burnt fraction and small clusters
    else if (burntFraction < 0.1 && largestClusterRatio < 0.05) {
      SubCritical
    }
    // Critical: intermediate values, especially near percolation threshold
    else {
      Critical
    }
  }
  
  /**
   * Identify phase from ensemble statistics
   */
  def identifyFromEnsemble(states: List[SimulationState]): Phase = {
    if (states.isEmpty) return SubCritical
    
    // Calculate average order parameters
    val avgBurnt = states.map(OrderParameters.burntFraction).sum / states.length
    val avgLargest = states.map(OrderParameters.largestClusterRatio).sum / states.length
    val avgPercolation = states.map(OrderParameters.percolationIndicator).sum / states.length
    
    // Also consider variance as indicator of criticality
    val burntValues = states.map(OrderParameters.burntFraction)
    val variance = calculateVariance(burntValues)
    val normalizedVariance = variance / (avgBurnt + 0.001) // Avoid division by zero
    
    // High variance indicates critical region
    if (normalizedVariance > 2.0 && avgBurnt > 0.05 && avgBurnt < 0.5) {
      Critical
    } else {
      identifyFromParameters(avgBurnt, avgLargest, avgPercolation)
    }
  }
  
  /**
   * Identify phase using cluster size distribution
   */
  def identifyFromClusterDistribution(
    clusterSizes: Map[Int, Int]
  ): Phase = {
    if (clusterSizes.isEmpty) return SubCritical
    
    val totalClusters = clusterSizes.values.sum
    val maxSize = clusterSizes.keys.max
    val sizes = clusterSizes.keys.toList.sorted
    
    // Check for power-law distribution (characteristic of critical phase)
    val powerLawFit = fitPowerLaw(clusterSizes)
    
    if (powerLawFit.r2 > 0.7 && powerLawFit.exponent > 1.0 && powerLawFit.exponent < 4.0) {
      Critical
    } else if (maxSize > 50 && clusterSizes.values.max.toDouble / totalClusters > 0.05) {
      SuperCritical
    } else {
      SubCritical
    }
  }
  
  /**
   * Calculate criticality score (0 to 1, with 1 being most critical)
   */
  def criticalityScore(state: SimulationState): Double = {
    val burntFrac = OrderParameters.burntFraction(state)
    val clusterDensity = OrderParameters.clusterDensity(state)
    val avgClusterSize = OrderParameters.averageClusterSize(state)
    
    // Criticality peaks at intermediate burnt fraction
    val burntScore = 4.0 * burntFrac * (1.0 - burntFrac)
    
    // High cluster density with moderate sizes indicates criticality
    val clusterScore = math.min(1.0, clusterDensity * 10.0) * 
                      math.exp(-math.abs(math.log(avgClusterSize + 1) - 2.0))
    
    (burntScore + clusterScore) / 2.0
  }
  
  /**
   * Detect phase transition from parameter sweep data
   */
  def detectTransition(
    parameterValues: List[Double],
    orderParameterValues: List[Double]
  ): Option[Double] = {
    if (parameterValues.length < 3) return None
    
    // Find steepest change in order parameter
    var maxDerivative = 0.0
    var transitionPoint = Option.empty[Double]
    
    for (i <- 1 until parameterValues.length - 1) {
      val derivative = (orderParameterValues(i+1) - orderParameterValues(i-1)) / 
                      (parameterValues(i+1) - parameterValues(i-1))
      
      if (math.abs(derivative) > maxDerivative) {
        maxDerivative = math.abs(derivative)
        transitionPoint = Some(parameterValues(i))
      }
    }
    
    // Only report transition if change is significant
    if (maxDerivative > 1.0) transitionPoint else None
  }
  
  // Helper methods
  
  private def calculateVariance(values: List[Double]): Double = {
    if (values.isEmpty) return 0.0
    val mean = values.sum / values.length
    values.map(x => math.pow(x - mean, 2)).sum / values.length
  }
  
  private case class PowerLawFit(exponent: Double, r2: Double)
  
  private def fitPowerLaw(distribution: Map[Int, Int]): PowerLawFit = {
    // Simple power law fitting using log-log regression
    val logData = distribution.collect {
      case (size, count) if size > 0 && count > 0 =>
        (math.log(size), math.log(count))
    }.toList
    
    if (logData.length < 3) return PowerLawFit(0.0, 0.0)
    
    val n = logData.length
    val sumX = logData.map(_._1).sum
    val sumY = logData.map(_._2).sum
    val sumXY = logData.map(p => p._1 * p._2).sum
    val sumX2 = logData.map(p => p._1 * p._1).sum
    
    val slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
    val intercept = (sumY - slope * sumX) / n
    
    // Calculate R²
    val meanY = sumY / n
    val ssTotal = logData.map(p => math.pow(p._2 - meanY, 2)).sum
    val ssResidual = logData.map { case (x, y) =>
      val predicted = slope * x + intercept
      math.pow(y - predicted, 2)
    }.sum
    
    val r2 = if (ssTotal > 0) 1.0 - ssResidual / ssTotal else 0.0
    
    PowerLawFit(-slope, r2) // Negative slope for decay
  }
}