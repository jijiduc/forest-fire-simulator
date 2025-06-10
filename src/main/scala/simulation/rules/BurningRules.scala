package simulation.rules

import models._
import simulation._
import cats.Monad
import cats.implicits._

/**
 * Rules for fire progression and behavior
 */
object BurningRules {
  
  /**
   * Update fire intensity based on conditions
   */
  case class IntensityEvolution[F[_]: Monad]() extends Rule[F] {
    override def name: String = "IntensityEvolution"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Burning
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      // Base intensity factors
      val fuelFactor = cell.vegetationType.fuelLoad
      val moistureFactor = 1.0 - cell.moisture
      val windFactor = 1.0 + state.climate.wind.speed * 0.05
      
      // Calculate slope effect
      val terrain = state.terrain
      val slope = terrain.slopeAt(cell.position.x, cell.position.y)
      val slopeFactor = 1.0 + slope * 0.5
      
      // Oxygen availability at elevation
      val elevation = cell.elevation
      // Simple oxygen factor based on elevation
      val oxygenFactor = math.max(0.5, 1.0 - (elevation - 1000.0) / 5000.0)
      
      // Calculate new temperature
      val baseTemp = 300.0 // Base fire temperature
      val maxTemp = 800.0  // Maximum fire temperature
      
      val intensityMultiplier = fuelFactor * moistureFactor * windFactor * 
                                slopeFactor * oxygenFactor
      
      val targetTemp = math.min(baseTemp * intensityMultiplier, maxTemp)
      
      // Gradual temperature change
      val tempChange = (targetTemp - cell.temperature) * 0.1 * state.timeStep
      val newTemp = cell.temperature + tempChange
      
      cell.copy(temperature = newTemp).pure[F]
    }
  }
  
  /**
   * Consume fuel based on burn rate
   */
  case class FuelConsumption[F[_]: Monad]() extends Rule[F] {
    override def name: String = "FuelConsumption"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Burning
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      // Calculate burn rate based on conditions
      val vegetationBurnRate = cell.vegetationType.burnRate
      val temperatureFactor = cell.temperature / 500.0
      val windFactor = 1.0 + state.climate.wind.speed * 0.02
      
      val burnRate = vegetationBurnRate * temperatureFactor * windFactor
      
      // Track fuel consumption (simplified as moisture increase)
      // In a more complex model, we'd track actual fuel mass
      val fuelConsumed = burnRate * state.timeStep * 0.01
      val newMoisture = math.min(cell.moisture + fuelConsumed, 0.95)
      
      // High moisture represents depleted fuel
      cell.copy(moisture = newMoisture).pure[F]
    }
  }
  
  /**
   * Generate heat for neighboring cells
   */
  case class HeatGeneration[F[_]: Monad]() extends Rule[F] {
    override def name: String = "HeatGeneration"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Burning && cell.temperature > 200.0
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      // Heat generation is handled by neighbor cells checking burning cells
      // This rule mainly ensures the burning cell maintains proper state
      
      // Add heat dissipation
      val ambientTemp = state.climate.season.baseTemperature
      val coolingRate = 0.05 * (cell.temperature - ambientTemp) * state.timeStep
      val newTemp = math.max(cell.temperature - coolingRate, ambientTemp)
      
      cell.copy(temperature = newTemp).pure[F]
    }
  }
  
  /**
   * Pre-heat neighboring cells (thermal radiation)
   */
  case class PreHeating[F[_]: Monad]() extends Rule[F] {
    override def name: String = "PreHeating"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Tree
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      val burningNeighbors = neighbors.filter(_.state == Burning)
      
      if (burningNeighbors.nonEmpty) {
        // Calculate heat transfer from burning neighbors
        val avgFireTemp = burningNeighbors.map(_.temperature).sum / burningNeighbors.length
        val heatTransferRate = 0.1 * state.timeStep
        
        // Consider distance and direction
        val windBonus = if (isDownwind(cell, burningNeighbors.head, state.climate.wind)) 1.5 else 1.0
        
        val tempIncrease = (avgFireTemp - cell.temperature) * heatTransferRate * windBonus * 0.1
        val newTemp = math.min(cell.temperature + tempIncrease, 100.0) // Cap pre-heating
        
        // Pre-heating also reduces moisture
        val moistureReduction = tempIncrease * 0.001
        val newMoisture = math.max(cell.moisture - moistureReduction, 0.0)
        
        cell.copy(
          temperature = newTemp,
          moisture = newMoisture
        ).pure[F]
      } else {
        cell.pure[F]
      }
    }
    
    private def isDownwind(cell: Cell, source: Cell, wind: Wind): Boolean = {
      val dx = cell.position.x - source.position.x
      val dy = cell.position.y - source.position.y
      val angle = math.atan2(dy, dx)
      val angleDiff = math.abs(angle - wind.direction)
      angleDiff < math.Pi / 4 // Within 45 degrees of wind direction
    }
  }
  
  /**
   * Smoke production tracking (for visualization)
   */
  case class SmokeProduction[F[_]: Monad]() extends Rule[F] {
    override def name: String = "SmokeProduction"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Burning
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      // Smoke production rate based on fuel type and intensity
      val smokeRate = cell.vegetationType.fuelLoad * (cell.temperature / 500.0)
      
      // For now, just maintain burning state
      // In a full implementation, we'd track smoke in a separate layer
      cell.pure[F]
    }
  }
}