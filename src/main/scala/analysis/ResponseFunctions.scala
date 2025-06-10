package analysis

import models._
import simulation._

/**
 * Response functions and susceptibilities for phase transition analysis
 */
object ResponseFunctions {
  
  /**
   * Calculate magnetic susceptibility (variance of order parameter)
   * χ = N * (<m²> - <m>²)
   */
  def magneticSusceptibility(
    ensemble: EnsembleResults
  ): Double = {
    val orderParam = "burntFraction"
    val values = ensemble.results.map(_.orderParameters.getOrElse(orderParam, 0.0))
    
    if (values.isEmpty) return 0.0
    
    val mean = values.sum / values.length
    val meanSquare = values.map(v => v * v).sum / values.length
    val systemSize = ensemble.results.headOption
      .map(r => r.finalState.grid.width * r.finalState.grid.height)
      .getOrElse(1)
    
    systemSize * (meanSquare - mean * mean)
  }
  
  /**
   * Calculate Binder cumulant
   * U = 1 - <m⁴>/(3<m²>²)
   * Universal at critical point, independent of system size
   */
  def binderCumulant(
    ensemble: EnsembleResults,
    orderParameter: String = "burntFraction"
  ): Double = {
    val values = ensemble.results.map(_.orderParameters.getOrElse(orderParameter, 0.0))
    
    if (values.isEmpty) return 0.0
    
    val m2 = values.map(v => v * v).sum / values.length
    val m4 = values.map(v => v * v * v * v).sum / values.length
    
    if (m2 > 0) {
      1.0 - m4 / (3.0 * m2 * m2)
    } else {
      0.0
    }
  }
  
  /**
   * Calculate higher-order cumulants for universality class identification
   */
  def cumulantRatios(
    ensemble: EnsembleResults,
    maxOrder: Int = 6
  ): Map[Int, Double] = {
    val values = ensemble.results.map(_.orderParameters.getOrElse("burntFraction", 0.0))
    
    if (values.isEmpty) return Map.empty
    
    // Calculate raw moments
    val moments = (1 to maxOrder).map { n =>
      n -> values.map(v => math.pow(v, n)).sum / values.length
    }.toMap
    
    // Calculate cumulants from moments
    val cumulants = calculateCumulants(moments)
    
    // Return ratios that are universal at criticality
    Map(
      4 -> (if (cumulants.contains(2) && cumulants(2) > 0) cumulants.getOrElse(4, 0.0) / math.pow(cumulants(2), 2) else 0.0),
      6 -> (if (cumulants.contains(2) && cumulants(2) > 0) cumulants.getOrElse(6, 0.0) / math.pow(cumulants(2), 3) else 0.0)
    )
  }
  
  /**
   * Calculate specific heat (variance of energy/action)
   */
  def specificHeat(
    ensemble: EnsembleResults
  ): Double = {
    // For forest fire model, "energy" could be total burnt area
    val energies = ensemble.results.map(_.finalState.metrics.totalBurntArea.toDouble)
    
    if (energies.isEmpty) return 0.0
    
    val mean = energies.sum / energies.length
    val variance = energies.map(e => math.pow(e - mean, 2)).sum / energies.length
    val systemSize = ensemble.results.headOption
      .map(r => r.finalState.grid.width * r.finalState.grid.height)
      .getOrElse(1)
    
    variance / systemSize
  }
  
  /**
   * Calculate connected susceptibility (cluster statistics)
   */
  def connectedSusceptibility(
    ensemble: EnsembleResults
  ): Double = {
    val clusterSizes = ensemble.results.map { result =>
      // Get average cluster size excluding the largest
      val sizes = extractClusterSizes(result.finalState)
      if (sizes.length > 1) {
        val sorted = sizes.sorted
        sorted.take(sorted.length - 1).sum.toDouble / sorted.length
      } else {
        0.0
      }
    }
    
    if (clusterSizes.isEmpty) return 0.0
    
    val mean = clusterSizes.sum / clusterSizes.length
    val variance = clusterSizes.map(s => math.pow(s - mean, 2)).sum / clusterSizes.length
    
    variance
  }
  
  /**
   * Calculate derivative of order parameter (for locating transitions)
   */
  def orderParameterDerivative(
    parameterPoints: List[ParameterPoint],
    orderParameter: String = "burntFraction"
  ): List[(Double, Double)] = {
    if (parameterPoints.length < 3) return List.empty
    
    val sorted = parameterPoints.sortBy(_.value)
    
    // Central difference approximation
    (1 until sorted.length - 1).map { i =>
      val p0 = sorted(i - 1)
      val p1 = sorted(i)
      val p2 = sorted(i + 1)
      
      val y0 = p0.ensemble.averageOrderParameters.getOrElse(orderParameter, 0.0)
      val y2 = p2.ensemble.averageOrderParameters.getOrElse(orderParameter, 0.0)
      
      val derivative = (y2 - y0) / (p2.value - p0.value)
      
      (p1.value, derivative)
    }.toList
  }
  
  /**
   * Calculate fluctuation-dissipation ratio
   */
  def fluctuationDissipationRatio(
    ensemble: EnsembleResults,
    externalField: Double = 0.01
  ): Double = {
    // This would require running with small external field
    // For now, return theoretical value
    val chi = magneticSusceptibility(ensemble)
    val systemSize = ensemble.results.headOption
      .map(r => r.finalState.grid.width * r.finalState.grid.height)
      .getOrElse(1)
    
    chi / systemSize // Simplified version
  }
  
  /**
   * Calculate correlation volume (ξ^d)
   */
  def correlationVolume(
    state: SimulationState,
    dimension: Int = 2
  ): Double = {
    val correlations = SpatialCorrelations.twoPointCorrelation(state, 20)
    val xi = SpatialCorrelations.correlationLength(correlations)
    
    math.pow(xi, dimension)
  }
  
  /**
   * Lee-Yang zeros analysis (simplified)
   */
  def leeYangZeros(
    ensemble: EnsembleResults
  ): List[Complex] = {
    // Simplified implementation - would need partition function
    // Return placeholder for now
    List(Complex(0.0, 1.0), Complex(0.0, -1.0))
  }
  
  // Helper methods
  
  private def calculateCumulants(moments: Map[Int, Double]): Map[Int, Double] = {
    // Convert raw moments to cumulants using the moment-cumulant formula
    Map(
      1 -> moments(1),
      2 -> (moments(2) - moments(1) * moments(1)),
      3 -> (moments(3) - 3 * moments(2) * moments(1) + 2 * math.pow(moments(1), 3)),
      4 -> (moments(4) - 4 * moments(3) * moments(1) - 3 * moments(2) * moments(2) + 
            12 * moments(2) * moments(1) * moments(1) - 6 * math.pow(moments(1), 4))
    )
  }
  
  private def extractClusterSizes(state: SimulationState): List[Int] = {
    val grid = state.grid
    val visited = scala.collection.mutable.Set[Position]()
    val clusterSizes = scala.collection.mutable.ListBuffer[Int]()
    
    def dfs(pos: Position): Int = {
      if (visited.contains(pos)) return 0
      
      grid.get(pos.x, pos.y) match {
        case Some(cell) if cell.state == Burning || cell.state == Burnt =>
          visited += pos
          var size = 1
          
          // Check neighbors
          for {
            dx <- -1 to 1
            dy <- -1 to 1
            if dx != 0 || dy != 0
            nx = pos.x + dx
            ny = pos.y + dy
            if nx >= 0 && nx < grid.width && ny >= 0 && ny < grid.height
            neighbor = Position(nx, ny)
            if !visited.contains(neighbor)
          } {
            size += dfs(neighbor)
          }
          
          size
        case _ => 
          visited += pos
          0
      }
    }
    
    // Find all clusters
    for {
      y <- 0 until grid.height
      x <- 0 until grid.width
      pos = Position(x, y)
      if !visited.contains(pos)
    } {
      val size = dfs(pos)
      if (size > 0) clusterSizes += size
    }
    
    clusterSizes.toList
  }
}

/**
 * Simple complex number for Lee-Yang zeros
 */
case class Complex(real: Double, imag: Double) {
  def +(other: Complex): Complex = Complex(real + other.real, imag + other.imag)
  def *(other: Complex): Complex = Complex(
    real * other.real - imag * other.imag,
    real * other.imag + imag * other.real
  )
  def magnitude: Double = math.sqrt(real * real + imag * imag)
  def phase: Double = math.atan2(imag, real)
}