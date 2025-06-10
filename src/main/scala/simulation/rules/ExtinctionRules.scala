package simulation.rules

import models._
import simulation._
import cats.Monad
import cats.implicits._

/**
 * Rules for fire extinction and termination
 */
object ExtinctionRules {
  
  /**
   * Fire dies when fuel is depleted
   */
  case class FuelDepletion[F[_]: Monad]() extends Rule[F] {
    override def name: String = "FuelDepletion"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Burning
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      // Use moisture as proxy for fuel depletion (high moisture = low fuel)
      val fuelThreshold = 0.9 // 90% moisture means fuel is depleted
      
      if (cell.moisture >= fuelThreshold) {
        cell.copy(
          state = Burnt,
          temperature = state.climate.season.baseTemperature + 50.0 // Still warm
        ).pure[F]
      } else {
        cell.pure[F]
      }
    }
  }
  
  /**
   * High moisture or precipitation suppresses fire
   */
  case class MoistureSuppression[F[_]: Monad]() extends Rule[F] {
    override def name: String = "MoistureSuppression"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Burning
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      val precipitation = state.climate.precipitation
      val humidity = state.climate.humidity
      
      // Calculate suppression factors
      val precipitationEffect = precipitation > 5.0 // Heavy rain
      val highMoisture = cell.moisture > 0.7
      val highHumidity = humidity > 0.8
      
      // Fire extinction probability
      val extinctionProb = 
        (if (precipitationEffect) 0.8 else 0.0) +
        (if (highMoisture) 0.3 else 0.0) +
        (if (highHumidity) 0.1 else 0.0)
      
      if (extinctionProb > 0) {
        val random = new scala.util.Random(
          (state.elapsedTime * 10000 + cell.position.x * 100 + cell.position.y).toLong
        )
        
        if (random.nextDouble() < extinctionProb) {
          cell.copy(
            state = Burnt,
            temperature = state.climate.season.baseTemperature
          ).pure[F]
        } else {
          // Increase moisture due to precipitation
          val moistureIncrease = precipitation * 0.01 * state.timeStep
          cell.copy(
            moisture = math.min(cell.moisture + moistureIncrease, 1.0)
          ).pure[F]
        }
      } else {
        cell.pure[F]
      }
    }
  }
  
  /**
   * Fire dies when temperature drops below threshold
   */
  case class TemperatureDecay[F[_]: Monad]() extends Rule[F] {
    override def name: String = "TemperatureDecay"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Burning
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      val ignitionThreshold = 150.0 // Below this, fire cannot sustain
      
      if (cell.temperature < ignitionThreshold) {
        cell.copy(
          state = Burnt,
          temperature = state.climate.season.baseTemperature
        ).pure[F]
      } else {
        cell.pure[F]
      }
    }
  }
  
  /**
   * Fire dies when isolated (no burning neighbors)
   */
  case class NeighborIsolation[F[_]: Monad]() extends Rule[F] {
    override def name: String = "NeighborIsolation"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Burning
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      val burningNeighbors = neighbors.count(_.state == Burning)
      
      // Isolated fires have higher extinction probability
      if (burningNeighbors == 0) {
        val isolationTime = 5.0 // Time steps before isolated fire dies
        val extinctionRate = state.timeStep / isolationTime
        
        val random = new scala.util.Random(
          (state.elapsedTime * 10000 + cell.position.x * 100 + cell.position.y).toLong
        )
        
        if (random.nextDouble() < extinctionRate) {
          cell.copy(
            state = Burnt,
            temperature = cell.temperature * 0.5 // Rapid cooling
          ).pure[F]
        } else {
          // Gradual temperature loss when isolated
          val coolingRate = 50.0 * state.timeStep
          cell.copy(
            temperature = math.max(cell.temperature - coolingRate, 100.0)
          ).pure[F]
        }
      } else {
        cell.pure[F]
      }
    }
  }
  
  /**
   * Calculate overall extinction probability
   */
  case class CombinedExtinction[F[_]: Monad]() extends Rule[F] {
    override def name: String = "CombinedExtinction"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Burning
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      // Use FireDynamics for comprehensive extinction calculation
      val extinctionProb = FireDynamics.calculateExtinctionProbability(
        cell,
        state.climate,
        neighbors.count(_.state == Burning)
      )
      
      val random = new scala.util.Random(
        (state.elapsedTime * 10000 + cell.position.x * 100 + cell.position.y).toLong
      )
      
      if (random.nextDouble() < extinctionProb) {
        cell.copy(
          state = Burnt,
          temperature = state.climate.season.baseTemperature + 20.0
        ).pure[F]
      } else {
        cell.pure[F]
      }
    }
  }
}