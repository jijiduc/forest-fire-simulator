package data

import models._
import simulation._
import java.time.Instant

/**
 * Comprehensive snapshot of simulation state for export and analysis
 */
case class SimulationSnapshot(
  timestamp: Double,
  simulationTime: Instant,
  grid: Grid,
  climate: Climate,
  terrain: Terrain,
  metrics: FireMetrics,
  spatialMetrics: SpatialMetrics,
  eventHistory: EventHistory,
  metadata: SnapshotMetadata
) {
  def withUpdatedMetrics(newMetrics: FireMetrics): SimulationSnapshot =
    copy(metrics = newMetrics)
    
  def withSpatialMetrics(spatial: SpatialMetrics): SimulationSnapshot =
    copy(spatialMetrics = spatial)
}

/**
 * Comprehensive fire metrics for analysis
 */
case class FireMetrics(
  // Basic fire statistics
  activeFires: Int,
  burntArea: Double,
  burningArea: Double,
  firePerimeter: Double,
  averageIntensity: Double,
  maxIntensity: Double,
  
  // Percolation metrics
  percolationIndicator: Double,
  largestClusterSize: Int,
  numberOfClusters: Int,
  clusterSizeDistribution: Map[Int, Int],
  
  // Environmental metrics
  averageMoisture: Double,
  averageTemperature: Double,
  averageWindSpeed: Double,
  treeDensity: Double,
  fuelLoad: Double,
  
  // Rate metrics
  spreadRate: Double,
  ignitionRate: Double,
  extinctionRate: Double,
  
  // Spatial correlations
  spatialCorrelationLength: Double,
  anisotropyFactor: Double
) {
  def burntFraction: Double = burntArea / (burntArea + burningArea + treeDensity)
  
  def fireIntensityVariance: Double = {
    // Would need cell-level data to compute properly
    0.0  // Placeholder
  }
}

/**
 * Spatial metrics for phase analysis
 */
case class SpatialMetrics(
  // Cluster analysis
  clusterPositions: List[ClusterInfo],
  clusterShapes: List[ClusterShape],
  
  // Spatial distributions
  fireIntensityMap: Array[Array[Double]],
  moistureMap: Array[Array[Double]],
  temperatureMap: Array[Array[Double]],
  
  // Correlation functions
  twoPointCorrelation: Array[Double],
  correlationDirections: Map[String, Array[Double]],
  
  // Front analysis
  fireFronts: List[FireFront],
  frontVelocities: List[Double],
  frontCurvatures: List[Double]
)

/**
 * Information about a fire cluster
 */
case class ClusterInfo(
  id: Int,
  size: Int,
  centerOfMass: (Double, Double),
  boundingBox: BoundingBox,
  perimeter: Int,
  compactness: Double,
  fractalDimension: Double
)

/**
 * Shape descriptor for clusters
 */
case class ClusterShape(
  clusterId: Int,
  aspectRatio: Double,
  eccentricity: Double,
  orientation: Double,
  isPercolating: Boolean
)

/**
 * Bounding box for spatial regions
 */
case class BoundingBox(
  minX: Int,
  minY: Int,
  maxX: Int,
  maxY: Int
) {
  def width: Int = maxX - minX + 1
  def height: Int = maxY - minY + 1
  def area: Int = width * height
}

/**
 * Fire front information
 */
case class FireFront(
  positions: List[Position],
  normal: (Double, Double),
  velocity: Double,
  curvature: Double,
  intensity: Double
)

/**
 * Event history for temporal analysis
 */
case class EventHistory(
  recentEvents: List[FireEvent],
  eventCounts: Map[FireEventType, Int],
  temporalDistribution: Map[Int, Int],  // Time bin -> count
  spatialDistribution: Map[Position, Int]  // Position -> event count
)

/**
 * Metadata for snapshot
 */
case class SnapshotMetadata(
  simulationId: String,
  configHash: String,
  version: String,
  hostname: String,
  createdAt: Instant,
  parameters: Map[String, String],
  notes: Option[String] = None
)

/**
 * Time series data structure for efficient storage
 */
case class TimeSeries[T](
  times: Vector[Double],
  values: Vector[T]
) {
  require(times.length == values.length, "Times and values must have same length")
  
  def add(time: Double, value: T): TimeSeries[T] =
    copy(times = times :+ time, values = values :+ value)
    
  def slice(startTime: Double, endTime: Double): TimeSeries[T] = {
    val indices = times.zipWithIndex.collect {
      case (t, i) if t >= startTime && t <= endTime => i
    }
    TimeSeries(
      indices.map(times(_)),
      indices.map(values(_))
    )
  }
  
  def downsample(factor: Int): TimeSeries[T] = {
    val sampledIndices = (0 until times.length by factor).toVector
    TimeSeries(
      sampledIndices.map(times(_)),
      sampledIndices.map(values(_))
    )
  }
}

/**
 * Container for multiple time series
 */
case class SimulationTimeSeries(
  metrics: TimeSeries[FireMetrics],
  snapshots: TimeSeries[SimulationSnapshot],
  events: TimeSeries[List[FireEvent]]
) {
  def duration: Double = 
    if (metrics.times.isEmpty) 0.0 
    else metrics.times.last - metrics.times.head
    
  def timeRange: (Double, Double) = 
    if (metrics.times.isEmpty) (0.0, 0.0)
    else (metrics.times.head, metrics.times.last)
}

/**
 * Factory methods for creating snapshots
 */
object SimulationSnapshot {
  def fromState(state: SimulationState, spatial: SpatialMetrics, metadata: SnapshotMetadata): SimulationSnapshot = {
    val metrics = convertMetrics(state.metrics, spatial)
    val eventHistory = EventHistory(
      recentEvents = state.eventLog.take(100),
      eventCounts = state.eventLog.groupBy {
        case _: IgnitionEvent => Ignition
        case _: ExtinctionEvent => Extinction
        case _: BurnoutEvent => Burnout
      }.mapValues(_.size).toMap,
      temporalDistribution = Map.empty,  // Would need time binning
      spatialDistribution = state.eventLog.groupBy(_.position).mapValues(_.size).toMap
    )
    
    SimulationSnapshot(
      timestamp = state.elapsedTime,
      simulationTime = Instant.now(),
      grid = state.grid,
      climate = state.climate,
      terrain = state.terrain,
      metrics = metrics,
      spatialMetrics = spatial,
      eventHistory = eventHistory,
      metadata = metadata
    )
  }
  
  private def convertMetrics(simMetrics: SimulationMetrics, spatial: SpatialMetrics): FireMetrics = {
    FireMetrics(
      activeFires = simMetrics.activeFires,
      burntArea = simMetrics.totalBurntArea.toDouble,
      burningArea = simMetrics.activeFires.toDouble,
      firePerimeter = calculatePerimeter(spatial),
      averageIntensity = simMetrics.averageFireIntensity,
      maxIntensity = spatial.fireIntensityMap.flatten.maxOption.getOrElse(0.0),
      percolationIndicator = simMetrics.percolationIndicator,
      largestClusterSize = simMetrics.largestFireClusterSize,
      numberOfClusters = spatial.clusterPositions.size,
      clusterSizeDistribution = Map.empty,  // Would need to compute
      averageMoisture = simMetrics.averageMoisture,
      averageTemperature = spatial.temperatureMap.flatten.sum / spatial.temperatureMap.flatten.length,
      averageWindSpeed = 0.0,  // Would need climate data
      treeDensity = simMetrics.treeDensity,
      fuelLoad = simMetrics.treeDensity * 10.0,  // Simple approximation
      spreadRate = 0.0,  // Would need temporal data
      ignitionRate = 0.0,  // Would need event data
      extinctionRate = 0.0,  // Would need event data
      spatialCorrelationLength = 0.0,  // Computed separately
      anisotropyFactor = 1.0  // Computed separately
    )
  }
  
  private def calculatePerimeter(spatial: SpatialMetrics): Double = {
    spatial.clusterPositions.map(_.perimeter).sum.toDouble
  }
}