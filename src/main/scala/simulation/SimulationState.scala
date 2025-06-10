package simulation

import models._

/**
 * Represents the complete state of the simulation at a given time
 */
case class SimulationState(
  grid: Grid,
  climate: Climate,
  terrain: Terrain,
  timeStep: Double,
  elapsedTime: Double,
  metrics: SimulationMetrics,
  eventLog: List[FireEvent]
) {
  def withGrid(newGrid: Grid): SimulationState = 
    copy(grid = newGrid)
    
  def withUpdatedTime(dt: Double): SimulationState = 
    copy(timeStep = dt, elapsedTime = elapsedTime + dt)
    
  def withMetrics(newMetrics: SimulationMetrics): SimulationState = 
    copy(metrics = newMetrics)
    
  def addEvent(event: FireEvent): SimulationState = 
    copy(eventLog = event :: eventLog)
    
  def addEvents(events: List[FireEvent]): SimulationState = 
    copy(eventLog = events ++ eventLog)
}

/**
 * Metrics collected during simulation
 */
case class SimulationMetrics(
  activeFires: Int,
  totalBurntArea: Int,
  largestFireClusterSize: Int,
  averageFireIntensity: Double,
  percolationIndicator: Double,
  treeDensity: Double,
  averageMoisture: Double
) {
  def fireRatio: Double = if (totalBurntArea > 0) activeFires.toDouble / totalBurntArea else 0.0
}

/**
 * Events that occur during simulation
 */
sealed trait FireEvent {
  def timestamp: Double
  def position: Position
}

case class IgnitionEvent(timestamp: Double, position: Position) extends FireEvent
case class ExtinctionEvent(timestamp: Double, position: Position) extends FireEvent
case class BurnoutEvent(timestamp: Double, position: Position) extends FireEvent

/**
 * Types of fire events
 */
sealed trait FireEventType
case object Ignition extends FireEventType
case object Extinction extends FireEventType
case object Burnout extends FireEventType

/**
 * Configuration for the simulation
 */
case class SimulationConfig(
  maxSteps: Int = 1000,
  maxTime: Double = 1000.0,
  adaptiveTimeStep: Boolean = true,
  minTimeStep: Double = 0.01,
  maxTimeStep: Double = 1.0,
  boundaryCondition: BoundaryCondition = AbsorbingBoundary,
  updateStrategy: UpdateStrategy = SynchronousUpdate,
  parallelism: Int = Runtime.getRuntime.availableProcessors(),
  randomSeed: Option[Long] = None
)

/**
 * Boundary conditions for the simulation
 */
sealed trait BoundaryCondition
case object PeriodicBoundary extends BoundaryCondition
case object ReflectiveBoundary extends BoundaryCondition
case object AbsorbingBoundary extends BoundaryCondition
case class FixedBoundary(conditions: Map[Position, CellState]) extends BoundaryCondition

/**
 * Update strategies for cell evolution
 */
sealed trait UpdateStrategy
case object SynchronousUpdate extends UpdateStrategy
case object AsynchronousUpdate extends UpdateStrategy
case class BlockUpdate(blockSize: Int) extends UpdateStrategy