package simulation.rules

import models._
import simulation._
import cats.Monad
import cats.implicits._
import scala.util.Random

/**
 * Rules for determining when cells catch fire
 */
object IgnitionRules {
  
  /**
   * Random ignition from sparks (e.g., lightning)
   */
  case class SparkIgnition[F[_]: Monad](sparkProbability: Double) extends Rule[F] {
    override def name: String = "SparkIgnition"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Tree && state.climate.season != Season.Winter
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      val random = new Random(
        (state.elapsedTime * 10000 + cell.position.x * 100 + cell.position.y).toLong
      )
      
      // Adjust probability based on conditions
      val adjustedProbability = sparkProbability * 
        (if (state.climate.humidity < 0.3) 2.0 else 1.0) * // Dry conditions
        (if (state.climate.precipitation > 0) 0.1 else 1.0) // Rain suppresses
      
      if (random.nextDouble() < adjustedProbability) {
        cell.copy(
          state = Burning,
          temperature = 300.0 // Initial fire temperature
        ).pure[F]
      } else {
        cell.pure[F]
      }
    }
  }
  
  /**
   * Fire spread from adjacent burning cells
   */
  case class NeighborIgnition[F[_]: Monad]() extends Rule[F] {
    override def name: String = "NeighborIgnition"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Tree
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      val burningNeighbors = neighbors.count(_.state == Burning)
      
      if (burningNeighbors > 0) {
        // Use FireDynamics for physically accurate ignition probability
        val ignitionProb = FireDynamics.calculateIgnitionProbability(
          cell, 
          state.climate, 
          state.terrain, 
          burningNeighbors
        )
        
        val random = new Random(
          (state.elapsedTime * 10000 + cell.position.x * 100 + cell.position.y).toLong
        )
        
        if (random.nextDouble() < ignitionProb * state.timeStep) {
          // Calculate pre-heating from neighbors
          val avgNeighborTemp = neighbors.filter(_.state == Burning)
            .map(_.temperature).sum / burningNeighbors
          
          cell.copy(
            state = Burning,
            temperature = 200.0 + (avgNeighborTemp - 200.0) * 0.3 // Pre-heated
          ).pure[F]
        } else {
          cell.pure[F]
        }
      } else {
        cell.pure[F]
      }
    }
  }
  
  /**
   * Long-distance ignition from flying embers
   */
  case class EmberIgnition[F[_]: Monad](maxDistance: Int) extends Rule[F] {
    override def name: String = "EmberIgnition"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Tree && state.climate.wind.speed > 5.0 // Only in windy conditions
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      // Check for fires within ember distance
      val windDir = state.climate.wind.direction
      val windSpeed = state.climate.wind.speed
      
      // Simple ember probability based on upwind fires
      val emberSources = countUpwindFires(cell, state, windDir, maxDistance)
      
      if (emberSources > 0) {
        val emberProbability = 0.001 * emberSources * (windSpeed / 10.0) * 
          (1.0 - cell.moisture) * // Dry cells more susceptible
          (if (cell.vegetationType.fuelLoad > 0.7) 1.5 else 1.0) // Dense vegetation
        
        val random = new Random(
          (state.elapsedTime * 10000 + cell.position.x * 100 + cell.position.y).toLong
        )
        
        if (random.nextDouble() < emberProbability) {
          cell.copy(
            state = Burning,
            temperature = 250.0 // Ember ignition temperature
          ).pure[F]
        } else {
          cell.pure[F]
        }
      } else {
        cell.pure[F]
      }
    }
    
    private def countUpwindFires(
      cell: Cell, 
      state: SimulationState, 
      windDir: Double,
      maxDist: Int
    ): Int = {
      // Simplified: count fires in upwind quadrant
      val grid = state.grid
      var count = 0
      
      for {
        dx <- -maxDist to maxDist
        dy <- -maxDist to maxDist
        if math.sqrt(dx * dx + dy * dy) <= maxDist
        x = cell.position.x + dx
        y = cell.position.y + dy
        if x >= 0 && x < grid.width && y >= 0 && y < grid.height
      } {
        val angle = math.atan2(-dy, dx) // Negative dy because y increases downward
        val angleDiff = math.abs(angle - windDir)
        val normalizedDiff = if (angleDiff > math.Pi) 2 * math.Pi - angleDiff else angleDiff
        
        // Check if in upwind direction (within 45 degrees)
        if (normalizedDiff < math.Pi / 4 && grid(x, y).state == Burning) {
          count += 1
        }
      }
      
      count
    }
  }
  
  /**
   * Human-caused ignition (future extension)
   */
  case class HumanIgnition[F[_]: Monad](
    ignitionPoints: List[Position],
    timeWindow: (Double, Double)
  ) extends Rule[F] {
    override def name: String = "HumanIgnition"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Tree && 
      ignitionPoints.contains(cell.position) &&
      state.elapsedTime >= timeWindow._1 && 
      state.elapsedTime <= timeWindow._2
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      if (isApplicable(cell, state)) {
        cell.copy(
          state = Burning,
          temperature = 300.0
        ).pure[F]
      } else {
        cell.pure[F]
      }
    }
  }
}