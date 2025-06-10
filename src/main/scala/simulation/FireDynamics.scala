package simulation

import models._
import scala.math._

/**
 * Fire dynamics model implementing physically accurate fire spread mechanics
 * Based on cellular automata with environmental factors
 */
object FireDynamics {

  /**
   * Parameters for fire dynamics calculations
   */
  case class FireDynamicsParameters(
    baseIgnitionProbability: Double = 0.01,
    moistureCoefficient: Double = 0.05,        // k_m in moisture factor
    temperatureCritical: Double = 30.0,        // Critical temperature in Celsius
    temperatureScale: Double = 5.0,            // Temperature transition scale
    slopeFactor: Double = 3.533,              // Empirical slope factor from literature
    windFactor: Double = 0.1783,              // Empirical wind factor from literature
    evaporationRate: Double = 0.001,          // k_evap
    precipitationRate: Double = 0.01,         // k_precip
    heatTransferRadius: Double = 2.0,         // Radius for radiative heat transfer
    convectionEnhancement: Double = 0.5,      // Upslope heat transfer boost
    oxygenReductionRate: Double = 0.00008    // Oxygen reduction per meter elevation
  )

  /**
   * Calculate ignition probability for a cell based on all environmental factors
   * P_ignite = P_base × M(moisture) × T(temp) × V(vegetation) × S(slope) × W(wind) × O(oxygen)
   */
  def calculateIgnitionProbability(
    cell: Cell,
    climate: Climate,
    terrain: Terrain,
    burningNeighbors: Int,
    params: FireDynamicsParameters = FireDynamicsParameters()
  ): Double = {
    
    // Base probability adjusted by burning neighbors
    val baseProbability = params.baseIgnitionProbability * (1 + burningNeighbors * 0.5)
    
    // Moisture factor: M(moisture) = exp(-k_m × moisture)
    val moistureFactor = exp(-params.moistureCoefficient * cell.moisture)
    
    // Temperature factor: sigmoid function
    val temperature = climate.temperatureAtElevation(cell.elevation)
    val temperatureFactor = 1.0 / (1.0 + exp(-(temperature - params.temperatureCritical) / params.temperatureScale))
    
    // Vegetation factor based on cell type
    val vegetationFactor = cell.vegetationType match {
      case VegetationType.DenseForest => 1.2
      case VegetationType.SparseForest => 1.0
      case VegetationType.Grassland => 0.8
      case VegetationType.Shrubland => 0.9
      case VegetationType.Barren => 0.1
      case VegetationType.Water => 0.0
      case VegetationType.Urban => 0.3
    }
    
    // Slope factor: K_φ = e^(3.533 × (tan(φ))^1.2)
    val slopeAngle = terrain.slopeAt(cell.position.x, cell.position.y)
    val slopeFactor = if (slopeAngle > 0 && slopeAngle < Pi/2) {
      exp(params.slopeFactor * pow(tan(slopeAngle), 1.2))
    } else {
      1.0
    }
    
    // Wind factor: K_w = e^(0.1783 × V)
    val windSpeed = climate.wind.speed
    val windFactor = exp(params.windFactor * windSpeed)
    
    // Oxygen factor based on elevation
    val oxygenFactor = climate.oxygenFactorAtElevation(cell.elevation)
    
    // Combine all factors
    val probability = baseProbability * 
                     moistureFactor * 
                     temperatureFactor * 
                     vegetationFactor * 
                     slopeFactor * 
                     windFactor * 
                     oxygenFactor
    
    // Ensure probability is in [0, 1]
    math.min(1.0, math.max(0.0, probability))
  }

  /**
   * Calculate heat transfer from burning cells to target cell
   */
  def calculateHeatTransfer(
    targetCell: Cell,
    burningCells: Seq[Cell],
    terrain: Terrain,
    climate: Climate,
    params: FireDynamicsParameters = FireDynamicsParameters()
  ): Double = {
    
    burningCells.map { burningCell =>
      val dx = targetCell.position.x - burningCell.position.x
      val dy = targetCell.position.y - burningCell.position.y
      val distance = sqrt(dx * dx + dy * dy)
      
      if (distance > 0 && distance <= params.heatTransferRadius) {
        // Radiative heat transfer (inverse square law)
        val radiativeHeat = burningCell.temperature / (distance * distance)
        
        // Convective heat transfer (enhanced upslope)
        val elevDiff = terrain.elevationAt(targetCell.position.x, targetCell.position.y) - 
                       terrain.elevationAt(burningCell.position.x, burningCell.position.y)
        val upslope = if (elevDiff > 0) {
          1.0 + params.convectionEnhancement * abs(elevDiff / distance)
        } else {
          1.0
        }
        
        // Wind enhancement in downwind direction
        val windEnhancement = if (climate.wind.dx * dx + climate.wind.dy * dy > 0) {
          1.0 + 0.2 * climate.wind.speed
        } else {
          1.0
        }
        
        radiativeHeat * upslope * windEnhancement
      } else {
        0.0
      }
    }.sum
  }

  /**
   * Update moisture content based on evaporation and precipitation
   * dM/dt = -k_evap × T × (1 - humidity) + k_precip × precipitation
   */
  def updateMoisture(
    currentMoisture: Double,
    temperature: Double,
    humidity: Double,
    precipitation: Double,
    dt: Double,
    params: FireDynamicsParameters = FireDynamicsParameters()
  ): Double = {
    
    // Evaporation: dM/dt = -k_evap × T × (1 - humidity)
    val evaporation = params.evaporationRate * temperature * (1.0 - humidity)
    
    // Precipitation absorption: dM/dt = +k_precip × precipitation
    val absorption = params.precipitationRate * precipitation
    
    // Update moisture
    val newMoisture = currentMoisture - evaporation * dt + absorption * dt
    
    // Ensure moisture is in [0, 1]
    math.min(1.0, math.max(0.0, newMoisture))
  }

  /**
   * Calculate fire spread rate based on environmental conditions
   */
  def calculateFireSpreadRate(
    cell: Cell,
    climate: Climate,
    terrain: Terrain,
    windSpeed: Double,
    params: FireDynamicsParameters = FireDynamicsParameters()
  ): Double = {
    
    // Base spread rate depends on vegetation type
    val baseRate = cell.vegetationType match {
      case VegetationType.DenseForest => 0.5
      case VegetationType.SparseForest => 0.7
      case VegetationType.Grassland => 1.2
      case VegetationType.Shrubland => 0.9
      case VegetationType.Barren => 0.1
      case VegetationType.Water => 0.0
      case VegetationType.Urban => 0.2
    }
    
    // Environmental modifiers
    val moistureModifier = exp(-params.moistureCoefficient * cell.moisture)
    val temperature = climate.temperatureAtElevation(cell.elevation)
    val temperatureModifier = 1.0 + 0.02 * (temperature - 20.0)
    val windModifier = exp(params.windFactor * windSpeed)
    val slopeModifier = 1.0 + sin(terrain.slopeAt(cell.position.x, cell.position.y))
    
    baseRate * moistureModifier * temperatureModifier * windModifier * slopeModifier
  }

  /**
   * Determine if a cell should transition from burning to burnt based on fuel depletion
   */
  def isFuelDepleted(cell: Cell, burnDuration: Double): Boolean = {
    val fuelContent = cell.vegetationType match {
      case VegetationType.DenseForest => 100.0
      case VegetationType.SparseForest => 60.0
      case VegetationType.Grassland => 20.0
      case VegetationType.Shrubland => 40.0
      case _ => 10.0
    }
    
    // Fuel consumption rate (assuming base consumption rate of 1.5 units per time)
    val baseConsumptionRate = 1.5
    val temperatureModifier = 1.0 + max(0.0, (cell.temperature - 20.0) / 100.0)
    val consumptionRate = baseConsumptionRate * temperatureModifier
    
    burnDuration * consumptionRate >= fuelContent
  }

  /**
   * Calculate the probability of fire extinction due to environmental factors
   */
  def calculateExtinctionProbability(
    cell: Cell,
    climate: Climate,
    precipitation: Double
  ): Double = {
    
    // High moisture increases extinction probability
    val moistureFactor = cell.moisture * 0.5
    
    // Low temperature increases extinction probability
    val temperature = climate.temperatureAtElevation(cell.elevation)
    val temperatureFactor = if (temperature < 10.0) {
      0.3 * (10.0 - temperature) / 10.0
    } else {
      0.0
    }
    
    // Precipitation directly contributes to extinction
    val precipitationFactor = precipitation * 0.8
    
    // High humidity helps extinction
    val humidityFactor = climate.humidity * 0.2
    
    math.min(1.0, moistureFactor + temperatureFactor + precipitationFactor + humidityFactor)
  }

  /**
   * Pre-heat adjacent cells based on proximity to fire
   */
  def preHeatCell(
    cell: Cell,
    heatTransfer: Double,
    dt: Double
  ): Cell = {
    // Simple heat capacity model
    val heatCapacity = cell.vegetationType match {
      case VegetationType.Water => 4.0
      case VegetationType.DenseForest => 2.0
      case VegetationType.SparseForest => 1.8
      case _ => 1.5
    }
    
    val temperatureIncrease = heatTransfer * dt / heatCapacity
    cell.copy(temperature = cell.temperature + temperatureIncrease)
  }

  /**
   * Calculate wind effect on fire spread direction
   */
  def calculateWindEffect(
    sourceCell: Cell,
    targetCell: Cell,
    wind: Wind
  ): Double = {
    val dx = targetCell.position.x - sourceCell.position.x
    val dy = targetCell.position.y - sourceCell.position.y
    val distance = sqrt(dx * dx + dy * dy)
    
    if (distance > 0) {
      // Normalize direction vector
      val dirX = dx / distance
      val dirY = dy / distance
      
      // Dot product with wind direction (cosine of angle)
      val windAlignment = dirX * wind.dx + dirY * wind.dy
      
      // Convert to probability modifier (1.0 for aligned, 0.5 for perpendicular, 0.2 for opposite)
      0.2 + 0.8 * (windAlignment + 1.0) / 2.0
    } else {
      1.0
    }
  }
}