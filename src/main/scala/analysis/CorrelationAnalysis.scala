package analysis

import models._
import simulation._
import scala.math._

/**
 * Data structure for directional correlation analysis
 */
case class DirectionalCorrelationData(
  horizontal: Map[Int, Double],
  vertical: Map[Int, Double],
  diagonal: Map[Int, Double],
  radial: Map[Int, Double],
  anisotropy: Double
)

/**
 * Spatial correlation analysis
 */
object SpatialCorrelations {
  
  /**
   * Calculate two-point correlation function
   * C(r) = <s_i * s_j> - <s_i><s_j> where |i-j| = r
   */
  def twoPointCorrelation(
    state: SimulationState,
    maxDistance: Int
  ): Map[Int, Double] = {
    val grid = state.grid
    val burningCells = grid.cells.flatten.map { cell =>
      if (cell.state == Burning || cell.state == Burnt) 1.0 else 0.0
    }
    
    val meanDensity = burningCells.sum / burningCells.length
    
    // Calculate correlation for each distance
    (1 to maxDistance).map { distance =>
      val correlations = for {
        y1 <- 0 until grid.height
        x1 <- 0 until grid.width
        (dx, dy) <- generateDistanceVectors(distance)
        x2 = x1 + dx
        y2 = y1 + dy
        if x2 >= 0 && x2 < grid.width && y2 >= 0 && y2 < grid.height
      } yield {
        val s1 = if (grid.get(x1, y1).exists(c => c.state == Burning || c.state == Burnt)) 1.0 else 0.0
        val s2 = if (grid.get(x2, y2).exists(c => c.state == Burning || c.state == Burnt)) 1.0 else 0.0
        s1 * s2
      }
      
      val meanCorr = if (correlations.nonEmpty) correlations.sum / correlations.length else 0.0
      distance -> (meanCorr - meanDensity * meanDensity)
    }.toMap
  }
  
  /**
   * Extract correlation length from correlation function
   */
  def correlationLength(
    correlations: Map[Int, Double]
  ): Double = {
    if (correlations.isEmpty) return 0.0
    
    // Find where correlation drops to 1/e of initial value
    val initial = correlations.getOrElse(1, 0.0)
    if (initial <= 0) return 0.0
    
    val threshold = initial / math.E
    
    correlations.find { case (r, c) => c < threshold } match {
      case Some((r, _)) => r.toDouble
      case None => correlations.keys.max.toDouble // Correlation extends beyond measured range
    }
  }
  
  /**
   * Analyze directional correlations to detect anisotropy
   */
  def directionalCorrelations(
    state: SimulationState
  ): DirectionalCorrelationData = {
    val maxDist = math.min(state.grid.width, state.grid.height) / 4
    
    // Calculate correlations in different directions
    val horizontal = horizontalCorrelation(state, maxDist)
    val vertical = verticalCorrelation(state, maxDist)
    val diagonal = diagonalCorrelation(state, maxDist)
    val radial = twoPointCorrelation(state, maxDist)
    
    // Measure anisotropy as ratio of max/min correlation lengths
    val lengths = List(
      correlationLength(horizontal),
      correlationLength(vertical),
      correlationLength(diagonal)
    ).filter(_ > 0)
    
    val anisotropy = if (lengths.nonEmpty) {
      lengths.max / (lengths.min + 1e-10)
    } else 1.0
    
    DirectionalCorrelationData(
      horizontal = horizontal,
      vertical = vertical,
      diagonal = diagonal,
      radial = radial,
      anisotropy = anisotropy
    )
  }
  
  /**
   * Calculate connected correlation function (cluster statistics)
   */
  def connectedCorrelation(
    state: SimulationState,
    maxDistance: Int
  ): Map[Int, Double] = {
    val clusters = findClusters(state.grid)
    
    (1 to maxDistance).map { distance =>
      val correlations = for {
        y1 <- 0 until state.grid.height
        x1 <- 0 until state.grid.width
        (dx, dy) <- generateDistanceVectors(distance)
        x2 = x1 + dx
        y2 = y1 + dy
        if x2 >= 0 && x2 < state.grid.width && y2 >= 0 && y2 < state.grid.height
      } yield {
        val cluster1 = clusters.find(_._2.contains(Position(x1, y1))).map(_._1)
        val cluster2 = clusters.find(_._2.contains(Position(x2, y2))).map(_._1)
        
        if (cluster1.isDefined && cluster1 == cluster2) 1.0 else 0.0
      }
      
      val meanCorr = if (correlations.nonEmpty) correlations.sum / correlations.length else 0.0
      distance -> meanCorr
    }.toMap
  }
  
  // Helper methods
  
  private def generateDistanceVectors(distance: Int): List[(Int, Int)] = {
    (for {
      dx <- -distance to distance
      dy <- -distance to distance
      if math.sqrt(dx * dx + dy * dy).round == distance
    } yield (dx, dy)).toList
  }
  
  private def horizontalCorrelation(state: SimulationState, maxDist: Int): Map[Int, Double] = {
    (1 to maxDist).map { dist =>
      val correlations = for {
        y <- 0 until state.grid.height
        x <- 0 until state.grid.width - dist
      } yield {
        val s1 = if (state.grid.get(x, y).exists(c => c.state == Burning || c.state == Burnt)) 1.0 else 0.0
        val s2 = if (state.grid.get(x + dist, y).exists(c => c.state == Burning || c.state == Burnt)) 1.0 else 0.0
        s1 * s2
      }
      
      val mean = if (correlations.nonEmpty) correlations.sum / correlations.length else 0.0
      dist -> mean
    }.toMap
  }
  
  private def verticalCorrelation(state: SimulationState, maxDist: Int): Map[Int, Double] = {
    (1 to maxDist).map { dist =>
      val correlations = for {
        y <- 0 until state.grid.height - dist
        x <- 0 until state.grid.width
      } yield {
        val s1 = if (state.grid.get(x, y).exists(c => c.state == Burning || c.state == Burnt)) 1.0 else 0.0
        val s2 = if (state.grid.get(x, y + dist).exists(c => c.state == Burning || c.state == Burnt)) 1.0 else 0.0
        s1 * s2
      }
      
      val mean = if (correlations.nonEmpty) correlations.sum / correlations.length else 0.0
      dist -> mean
    }.toMap
  }
  
  private def diagonalCorrelation(state: SimulationState, maxDist: Int): Map[Int, Double] = {
    (1 to maxDist).map { dist =>
      val correlations = for {
        y <- 0 until state.grid.height - dist
        x <- 0 until state.grid.width - dist
      } yield {
        val s1 = if (state.grid.get(x, y).exists(c => c.state == Burning || c.state == Burnt)) 1.0 else 0.0
        val s2 = if (state.grid.get(x + dist, y + dist).exists(c => c.state == Burning || c.state == Burnt)) 1.0 else 0.0
        s1 * s2
      }
      
      val mean = if (correlations.nonEmpty) correlations.sum / correlations.length else 0.0
      dist -> mean
    }.toMap
  }
  
  private def findClusters(grid: Grid): Map[Int, Set[Position]] = {
    val visited = scala.collection.mutable.Set[Position]()
    val clusters = scala.collection.mutable.Map[Int, Set[Position]]()
    var clusterId = 0
    
    def dfs(pos: Position, cluster: scala.collection.mutable.Set[Position]): Unit = {
      if (visited.contains(pos)) return
      
      grid.get(pos.x, pos.y) match {
        case Some(cell) if cell.state == Burning || cell.state == Burnt =>
          visited += pos
          cluster += pos
          
          // Check neighbors
          for {
            dx <- -1 to 1
            dy <- -1 to 1
            if dx != 0 || dy != 0
            neighbor = Position(pos.x + dx, pos.y + dy)
            if !visited.contains(neighbor)
          } dfs(neighbor, cluster)
          
        case _ => ()
      }
    }
    
    // Find all clusters
    for {
      y <- 0 until grid.height
      x <- 0 until grid.width
      pos = Position(x, y)
      if !visited.contains(pos)
    } {
      grid.get(x, y) match {
        case Some(cell) if cell.state == Burning || cell.state == Burnt =>
          val cluster = scala.collection.mutable.Set[Position]()
          dfs(pos, cluster)
          if (cluster.nonEmpty) {
            clusters(clusterId) = cluster.toSet
            clusterId += 1
          }
        case _ => ()
      }
    }
    
    clusters.toMap
  }
}

/**
 * Temporal correlation analysis
 */
object TemporalCorrelations {
  
  /**
   * Calculate autocorrelation function for a time series
   */
  def autocorrelation(
    timeSeries: List[Map[String, Double]],
    parameter: String,
    maxLag: Int
  ): Map[Int, Double] = {
    if (timeSeries.isEmpty) return Map.empty
    
    val values = timeSeries.map(_.getOrElse(parameter, 0.0))
    val mean = values.sum / values.length
    val variance = values.map(v => math.pow(v - mean, 2)).sum / values.length
    
    if (variance < 1e-10) return (0 to maxLag).map(_ -> 1.0).toMap
    
    (0 to math.min(maxLag, values.length - 1)).map { lag =>
      val covariance = (0 until values.length - lag).map { t =>
        (values(t) - mean) * (values(t + lag) - mean)
      }.sum / (values.length - lag)
      
      lag -> (covariance / variance)
    }.toMap
  }
  
  /**
   * Calculate dynamic critical exponent z from relaxation times
   * τ ~ L^z at criticality
   */
  def dynamicExponent(
    relaxationTimes: Map[Int, Double],
    systemSizes: List[Int]
  ): ExponentWithError = {
    if (relaxationTimes.size < 2) {
      return ExponentWithError(2.0, 0.5, "insufficient_data", 0.0)
    }
    
    // Log-log fit
    val logData = systemSizes.flatMap { L =>
      relaxationTimes.get(L).map(tau => (math.log(L), math.log(tau)))
    }.filter(p => p._1.isFinite && p._2.isFinite)
    
    if (logData.length < 2) {
      return ExponentWithError(2.0, 0.5, "insufficient_data", 0.0)
    }
    
    val (slope, error, r2) = linearRegression(logData)
    
    ExponentWithError(slope, error, "log-log regression", r2)
  }
  
  /**
   * Extract relaxation time from autocorrelation
   */
  def relaxationTime(autocorr: Map[Int, Double]): Double = {
    // Find where autocorrelation drops to 1/e
    val threshold = 1.0 / math.E
    
    autocorr.find { case (lag, corr) => corr < threshold } match {
      case Some((lag, _)) => lag.toDouble
      case None => autocorr.keys.max.toDouble
    }
  }
  
  /**
   * Calculate power spectral density
   */
  def powerSpectralDensity(
    timeSeries: List[Double]
  ): Map[Double, Double] = {
    val n = timeSeries.length
    if (n < 2) return Map.empty
    
    // Simple DFT (in practice, would use FFT)
    val frequencies = (1 until n/2).map(_.toDouble / n)
    
    frequencies.map { freq =>
      val omega = 2 * math.Pi * freq
      val real = timeSeries.zipWithIndex.map { case (x, t) =>
        x * math.cos(omega * t)
      }.sum
      val imag = timeSeries.zipWithIndex.map { case (x, t) =>
        x * math.sin(omega * t)
      }.sum
      
      val power = (real * real + imag * imag) / n
      freq -> power
    }.toMap
  }
  
  /**
   * Analyze critical slowing down
   */
  def criticalSlowingDown(
    timeSeries: List[Map[String, Double]],
    parameter: String,
    windowSize: Int = 50
  ): List[Double] = {
    if (timeSeries.length < windowSize) return List.empty
    
    timeSeries.sliding(windowSize).map { window =>
      val values = window.map(_.getOrElse(parameter, 0.0))
      val variance = calculateVariance(values)
      val autocorr = autocorrelation(window, parameter, 10)
      val relaxTime = relaxationTime(autocorr)
      
      variance * relaxTime // Critical slowing down indicator
    }.toList
  }
  
  // Helper methods
  
  private def calculateVariance(values: List[Double]): Double = {
    if (values.isEmpty) return 0.0
    val mean = values.sum / values.length
    values.map(v => math.pow(v - mean, 2)).sum / values.length
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
    val ssTotal = data.map(p => math.pow(p._2 - meanY, 2)).sum
    val ssResidual = data.map { case (x, y) =>
      val predicted = slope * x + intercept
      math.pow(y - predicted, 2)
    }.sum
    
    val r2 = if (ssTotal > 0) 1.0 - ssResidual / ssTotal else 0.0
    
    // Estimate error
    val se = if (n > 2) {
      math.sqrt(ssResidual / (n - 2)) / math.sqrt(sumX2 - sumX * sumX / n)
    } else 1.0
    
    (slope, se, r2)
  }
}