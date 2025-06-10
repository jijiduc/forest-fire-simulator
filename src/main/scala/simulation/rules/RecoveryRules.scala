package simulation.rules

import models._
import simulation._
import cats.Monad
import cats.implicits._
import scala.util.Random

/**
 * Rules for natural vegetation recovery after fire
 */
object RecoveryRules {
  
  /**
   * Natural regrowth of vegetation over time
   */
  case class NaturalRegrowth[F[_]: Monad](regrowthRate: Double) extends Rule[F] {
    override def name: String = "NaturalRegrowth"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Burnt || cell.state == Empty
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      // Base regrowth probability
      val baseProb = regrowthRate * state.timeStep
      
      // Environmental factors
      val moisture = cell.moisture
      val temperature = state.climate.season.baseTemperature
      val elevation = cell.elevation
      
      // Optimal conditions for regrowth
      val moistureFactor = if (moisture > 0.3 && moisture < 0.8) 1.5 else 0.5
      val tempFactor = if (temperature > 10 && temperature < 25) 1.2 else 0.8
      val elevationFactor = if (elevation < 2500) 1.0 else 0.5 // Harder at high altitude
      
      // Neighbor influence (seeds from nearby vegetation)
      val treeNeighbors = neighbors.count(_.state == Tree)
      val neighborFactor = 1.0 + (treeNeighbors * 0.2)
      
      val finalProb = baseProb * moistureFactor * tempFactor * elevationFactor * neighborFactor
      
      val random = new Random(
        (state.elapsedTime * 10000 + cell.position.x * 100 + cell.position.y).toLong
      )
      
      if (random.nextDouble() < finalProb) {
        // Determine vegetation type based on elevation
        val vegetationType = VegetationType.fromElevation(elevation)
        
        cell.copy(
          state = Tree,
          vegetationType = vegetationType,
          moisture = 0.5, // Reset to healthy moisture
          temperature = state.climate.season.baseTemperature
        ).pure[F]
      } else {
        cell.pure[F]
      }
    }
  }
  
  /**
   * Enhanced growth during favorable seasons
   */
  case class SeasonalGrowth[F[_]: Monad]() extends Rule[F] {
    override def name: String = "SeasonalGrowth"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      (cell.state == Burnt || cell.state == Empty) && 
      (state.climate.season == Season.Spring || state.climate.season == Season.Summer)
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      val seasonBonus = state.climate.season match {
        case Season.Spring => 3.0 // High growth in spring
        case Season.Summer => 2.0 // Good growth in summer
        case _ => 1.0
      }
      
      // Higher precipitation helps
      val precipBonus = if (state.climate.precipitation > 2.0) 1.5 else 1.0
      
      val enhancedRate = 0.001 * seasonBonus * precipBonus * state.timeStep
      
      val random = new Random(
        (state.elapsedTime * 10000 + cell.position.x * 100 + cell.position.y).toLong
      )
      
      if (random.nextDouble() < enhancedRate) {
        val vegetationType = VegetationType.fromElevation(cell.elevation)
        
        cell.copy(
          state = Tree,
          vegetationType = vegetationType,
          moisture = 0.6, // Spring/summer moisture
          temperature = state.climate.season.baseTemperature
        ).pure[F]
      } else {
        cell.pure[F]
      }
    }
  }
  
  /**
   * Recovery influenced by seed dispersion from nearby vegetation
   */
  case class SeedDispersion[F[_]: Monad]() extends Rule[F] {
    override def name: String = "SeedDispersion"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Burnt || cell.state == Empty
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      // Count healthy vegetation in extended neighborhood
      val healthyNeighbors = neighbors.filter(n => n.state == Tree && n.moisture > 0.3)
      
      if (healthyNeighbors.nonEmpty) {
        // Wind-assisted seed dispersion
        val windSpeed = state.climate.wind.speed
        val dispersionBonus = 1.0 + (windSpeed / 20.0)
        
        // Most common vegetation type among neighbors
        val dominantVegetation = healthyNeighbors
          .groupBy(_.vegetationType)
          .maxBy(_._2.length)
          ._1
        
        val seedProb = 0.0005 * healthyNeighbors.length * dispersionBonus * state.timeStep
        
        val random = new Random(
          (state.elapsedTime * 10000 + cell.position.x * 100 + cell.position.y).toLong
        )
        
        if (random.nextDouble() < seedProb) {
          cell.copy(
            state = Tree,
            vegetationType = dominantVegetation,
            moisture = 0.4,
            temperature = state.climate.season.baseTemperature
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
   * Succession stages - different vegetation types over time
   */
  case class VegetationSuccession[F[_]: Monad]() extends Rule[F] {
    override def name: String = "VegetationSuccession"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Tree
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      // Simple succession model: grass -> shrub -> sparse forest -> dense forest
      val currentType = cell.vegetationType
      val elevation = cell.elevation
      
      // Time-based succession (very slow process)
      val successionRate = 0.00001 * state.timeStep
      
      val random = new Random(
        (state.elapsedTime * 10000 + cell.position.x * 100 + cell.position.y).toLong
      )
      
      if (random.nextDouble() < successionRate) {
        val nextType = currentType match {
          case VegetationType.Grassland => VegetationType.Shrubland
          case VegetationType.Shrubland => VegetationType.SparseForest
          case VegetationType.SparseForest => 
            if (elevation < 2000) VegetationType.DenseForest 
            else VegetationType.SparseForest
          case _ => currentType
        }
        
        cell.copy(vegetationType = nextType).pure[F]
      } else {
        cell.pure[F]
      }
    }
  }
  
  /**
   * Moisture recovery in burnt areas
   */
  case class MoistureRecovery[F[_]: Monad]() extends Rule[F] {
    override def name: String = "MoistureRecovery"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      cell.state == Burnt && cell.moisture < 0.5
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = {
      // Gradual moisture recovery from precipitation and humidity
      val precipRate = state.climate.precipitation * 0.01
      val humidityRate = state.climate.humidity * 0.005
      val recoveryRate = (precipRate + humidityRate) * state.timeStep
      
      val newMoisture = math.min(cell.moisture + recoveryRate, 0.8)
      
      cell.copy(moisture = newMoisture).pure[F]
    }
  }
}