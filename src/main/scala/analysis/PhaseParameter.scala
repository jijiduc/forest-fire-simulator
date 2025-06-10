package analysis

import models._
import simulation._

/**
 * Represents a controllable parameter for phase transition analysis
 */
sealed trait PhaseParameter {
  def name: String
  def unit: String
  def applyToState(state: SimulationState, value: Double): SimulationState
  def applyToGrid(grid: Grid, value: Double, terrain: Terrain, climate: Climate): Grid
}

/**
 * Tree density parameter - controls the fraction of cells containing trees
 */
case object TreeDensityParameter extends PhaseParameter {
  val name = "Tree Density"
  val unit = "fraction"
  
  def applyToState(state: SimulationState, value: Double): SimulationState = {
    val newGrid = applyToGrid(state.grid, value, state.terrain, state.climate)
    state.copy(grid = newGrid)
  }
  
  def applyToGrid(grid: Grid, density: Double, terrain: Terrain, climate: Climate): Grid = {
    require(density >= 0.0 && density <= 1.0, s"Tree density must be in [0,1], got $density")
    
    val random = new scala.util.Random(42) // Fixed seed for reproducibility
    val cells = grid.cells.map { row =>
      row.map { cell =>
        if (cell.state == Empty || cell.state == Tree) {
          // Only modify non-burning/burnt cells
          if (random.nextDouble() < density && cell.vegetationType != VegetationType.Water) {
            cell.copy(state = Tree)
          } else {
            cell.copy(state = Empty)
          }
        } else {
          cell
        }
      }
    }
    
    grid.copy(cells = cells)
  }
}

/**
 * Average moisture parameter - controls the moisture content of all cells
 */
case object MoistureParameter extends PhaseParameter {
  val name = "Average Moisture"
  val unit = "fraction"
  
  def applyToState(state: SimulationState, value: Double): SimulationState = {
    require(value >= 0.0 && value <= 1.0, s"Moisture must be in [0,1], got $value")
    
    val newGrid = state.grid.map { cell =>
      cell.copy(moisture = value)
    }
    
    state.copy(grid = newGrid)
  }
  
  def applyToGrid(grid: Grid, moisture: Double, terrain: Terrain, climate: Climate): Grid = {
    grid.map { cell =>
      cell.copy(moisture = moisture)
    }
  }
}

/**
 * Wind speed parameter - controls the wind velocity
 */
case object WindSpeedParameter extends PhaseParameter {
  val name = "Wind Speed"
  val unit = "m/s"
  
  def applyToState(state: SimulationState, value: Double): SimulationState = {
    require(value >= 0.0, s"Wind speed must be non-negative, got $value")
    
    val newWind = state.climate.wind.copy(speed = value)
    val newClimate = state.climate.copy(wind = newWind)
    state.copy(climate = newClimate)
  }
  
  def applyToGrid(grid: Grid, value: Double, terrain: Terrain, climate: Climate): Grid = {
    // Wind doesn't directly modify grid
    grid
  }
}

/**
 * Temperature parameter - controls the base temperature
 */
case object TemperatureParameter extends PhaseParameter {
  val name = "Temperature Anomaly"
  val unit = "°C"
  
  def applyToState(state: SimulationState, value: Double): SimulationState = {
    // Value represents temperature anomaly from seasonal baseline
    val baseTemp = state.climate.season.baseTemperature
    val newTemp = baseTemp + value
    
    // Update all cell temperatures accordingly
    val newGrid = state.grid.map { cell =>
      val cellTemp = state.climate.temperatureAtElevation(cell.elevation) + value
      cell.copy(temperature = cellTemp)
    }
    
    state.copy(grid = newGrid)
  }
  
  def applyToGrid(grid: Grid, value: Double, terrain: Terrain, climate: Climate): Grid = {
    grid.map { cell =>
      val cellTemp = climate.temperatureAtElevation(cell.elevation) + value
      cell.copy(temperature = cellTemp)
    }
  }
}

/**
 * Spark probability parameter - controls random ignition rate
 */
case object SparkProbabilityParameter extends PhaseParameter {
  val name = "Spark Probability"
  val unit = "probability/step"
  
  def applyToState(state: SimulationState, value: Double): SimulationState = {
    require(value >= 0.0 && value <= 1.0, s"Spark probability must be in [0,1], got $value")
    // This parameter affects the rule configuration, not the state directly
    state
  }
  
  def applyToGrid(grid: Grid, value: Double, terrain: Terrain, climate: Climate): Grid = {
    // Spark probability doesn't directly modify grid
    grid
  }
}

/**
 * Helper object for parameter operations
 */
object PhaseParameter {
  val all: List[PhaseParameter] = List(
    TreeDensityParameter,
    MoistureParameter,
    WindSpeedParameter,
    TemperatureParameter,
    SparkProbabilityParameter
  )
  
  def fromName(name: String): Option[PhaseParameter] = 
    all.find(_.name == name)
}

/**
 * Range specification for parameter sweeps
 */
case class ParameterRange(min: Double, max: Double, steps: Int) {
  require(min <= max, s"Min ($min) must be <= max ($max)")
  require(steps > 0, s"Steps must be positive, got $steps")
  
  def values: List[Double] = {
    if (steps == 1) List(min)
    else (0 until steps).map { i =>
      min + (max - min) * i / (steps - 1)
    }.toList
  }
  
  def contains(value: Double): Boolean = value >= min && value <= max
}