package simulation

import cats.effect._
import cats.implicits._
import fs2.Stream
import scala.concurrent.duration._
import scala.util.Random
import models._

/**
 * IO-based implementation of the simulation engine
 * Provides pure functional simulation with controlled side effects
 */
class IOSimulationEngine(
  timeStepCalculator: TimeStepCalculator = AdaptiveTimeStep
) extends SimulationEngine[IO] {
  
  /**
   * Execute a single simulation step
   */
  override def step(state: SimulationState, config: SimulationConfig): IO[SimulationState] = {
    for {
      // Calculate adaptive time step
      dt <- IO(timeStepCalculator.calculateDt(state, config))
      
      // Update grid based on strategy
      (newGrid, events) <- config.updateStrategy match {
        case SynchronousUpdate => synchronousUpdate(state, dt, config)
        case AsynchronousUpdate => asynchronousUpdate(state, dt, config)
        case BlockUpdate(blockSize) => blockUpdate(state, dt, config, blockSize)
      }
      
      // Collect metrics
      newState = state.copy(grid = newGrid, timeStep = dt, elapsedTime = state.elapsedTime + dt)
      metrics <- IO(MetricsCollector.collectMetrics(newState))
      
      // Update events with proper timestamps
      timestampedEvents = events.map {
        case IgnitionEvent(_, pos) => IgnitionEvent(newState.elapsedTime, pos)
        case ExtinctionEvent(_, pos) => ExtinctionEvent(newState.elapsedTime, pos)
        case BurnoutEvent(_, pos) => BurnoutEvent(newState.elapsedTime, pos)
      }
      
    } yield newState
      .withMetrics(metrics)
      .addEvents(timestampedEvents)
  }
  
  /**
   * Run simulation for a fixed number of steps
   */
  override def run(
    initial: SimulationState, 
    steps: Int, 
    config: SimulationConfig
  ): Stream[IO, SimulationState] = {
    Stream.iterateEval(initial) { state =>
      step(state, config)
    }.take(steps + 1)
  }
  
  /**
   * Run simulation until a condition is met
   */
  override def runUntil(
    initial: SimulationState, 
    condition: SimulationState => Boolean,
    config: SimulationConfig
  ): Stream[IO, SimulationState] = {
    Stream.iterateEval(initial) { state =>
      step(state, config)
    }.takeWhile(state => !condition(state)) ++ Stream.emit(initial).covary[IO]
  }
  
  /**
   * Run simulation with adaptive time stepping until maxTime
   */
  override def runAdaptive(
    initial: SimulationState, 
    maxTime: Double,
    config: SimulationConfig
  ): Stream[IO, SimulationState] = {
    def iterate(state: SimulationState): Stream[IO, SimulationState] = {
      if (state.elapsedTime >= maxTime) {
        Stream.empty
      } else {
        Stream.eval(step(state, config)).flatMap { newState =>
          val adjustedState = if (newState.elapsedTime > maxTime) {
            // Adjust last step to not overshoot maxTime
            val adjustedDt = maxTime - state.elapsedTime
            newState.copy(
              timeStep = adjustedDt,
              elapsedTime = maxTime
            )
          } else {
            newState
          }
          Stream.emit(adjustedState) ++ iterate(adjustedState)
        }
      }
    }
    
    Stream.emit(initial) ++ iterate(initial)
  }
  
  /**
   * Synchronous update - all cells updated simultaneously
   */
  private def synchronousUpdate(
    state: SimulationState, 
    dt: Double, 
    config: SimulationConfig
  ): IO[(Grid, List[FireEvent])] = IO {
    val random = config.randomSeed.map(new Random(_)).getOrElse(new Random())
    
    // Prepare cell-neighbor pairs
    val cellUpdates = for {
      y <- 0 until state.grid.height
      x <- 0 until state.grid.width
    } yield {
      val cell = state.grid(x, y)
      val neighbors = BoundaryHandler.getNeighborsWithBoundary(
        state.grid, x, y, config.boundaryCondition
      )
      (cell, neighbors)
    }
    
    // Update all cells (parallel collection not available in Scala 3)
    val updates = cellUpdates.map { case (cell, neighbors) =>
      CellUpdateLogic.updateCell(
        cell, neighbors, state.climate, state.terrain, dt,
        FireDynamics.FireDynamicsParameters(), random
      )
    }
    
    // Reconstruct grid and collect events
    val (updatedCells, eventLists) = updates.unzip
    val newGrid = Grid(
      updatedCells.grouped(state.grid.width).map(_.toVector).toVector,
      state.grid.width,
      state.grid.height
    )
    
    (newGrid, eventLists.flatten.toList)
  }
  
  /**
   * Asynchronous update - cells updated in random order
   */
  private def asynchronousUpdate(
    state: SimulationState, 
    dt: Double, 
    config: SimulationConfig
  ): IO[(Grid, List[FireEvent])] = IO {
    val random = config.randomSeed.map(new Random(_)).getOrElse(new Random())
    var currentGrid = state.grid
    val events = scala.collection.mutable.ListBuffer[FireEvent]()
    
    // Create random update order
    val positions = for {
      y <- 0 until state.grid.height
      x <- 0 until state.grid.width
    } yield (x, y)
    
    val shuffledPositions = random.shuffle(positions)
    
    // Update cells one by one
    shuffledPositions.foreach { case (x, y) =>
      val cell = currentGrid(x, y)
      val neighbors = BoundaryHandler.getNeighborsWithBoundary(
        currentGrid, x, y, config.boundaryCondition
      )
      
      val (updatedCell, cellEvents) = CellUpdateLogic.updateCell(
        cell, neighbors, state.climate, state.terrain, dt,
        FireDynamics.FireDynamicsParameters(), random
      )
      
      currentGrid = currentGrid.updated(x, y, updatedCell)
      events ++= cellEvents
    }
    
    (currentGrid, events.toList)
  }
  
  /**
   * Block update - divide grid into blocks for parallel processing
   */
  private def blockUpdate(
    state: SimulationState, 
    dt: Double, 
    config: SimulationConfig,
    blockSize: Int
  ): IO[(Grid, List[FireEvent])] = {
    // For simplicity, fall back to synchronous update
    // Full block update would require more complex synchronization
    synchronousUpdate(state, dt, config)
  }
}

/**
 * Companion object for creating simulation engines
 */
object IOSimulationEngine {
  def apply(): IOSimulationEngine = new IOSimulationEngine()
  
  def withTimeStepCalculator(calculator: TimeStepCalculator): IOSimulationEngine = 
    new IOSimulationEngine(calculator)
}