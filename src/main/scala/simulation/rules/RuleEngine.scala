package simulation.rules

import models._
import simulation.SimulationState
import cats.Monad
import cats.implicits._

/**
 * Engine for orchestrating rule application
 */
class RuleEngine[F[_]: Monad](
  ignitionRules: List[Rule[F]],
  burningRules: List[Rule[F]],
  extinctionRules: List[Rule[F]],
  recoveryRules: List[Rule[F]],
  interventionRules: List[Rule[F]]
) {
  
  /**
   * Apply all applicable rules to a cell
   */
  def applyRules(
    cell: Cell, 
    neighbors: List[Cell], 
    state: SimulationState
  ): F[Cell] = {
    // Select rules based on cell state
    val applicableRules = selectApplicableRules(cell, state)
    
    // Apply rules in sequence
    applicableRules.foldLeftM(cell) { (currentCell, rule) =>
      if (rule.isApplicable(currentCell, state))
        rule.apply(currentCell, neighbors, state)
      else
        currentCell.pure[F]
    }
  }
  
  /**
   * Select rules applicable to the current cell state
   */
  def selectApplicableRules(cell: Cell, state: SimulationState): List[Rule[F]] = {
    cell.state match {
      case Empty => recoveryRules ++ interventionRules
      case Tree => ignitionRules ++ interventionRules
      case Burning => burningRules ++ extinctionRules ++ interventionRules
      case Burnt => recoveryRules ++ interventionRules
    }
  }
  
  /**
   * Get all configured rules
   */
  def allRules: List[Rule[F]] = 
    ignitionRules ++ burningRules ++ extinctionRules ++ recoveryRules ++ interventionRules
}

object RuleEngine {
  /**
   * Create a default rule engine with standard rules
   */
  def default[F[_]: Monad](config: RuleConfig): RuleEngine[F] = {
    import IgnitionRules._
    import BurningRules._
    import ExtinctionRules._
    import RecoveryRules._
    import InterventionRules._
    
    new RuleEngine[F](
      ignitionRules = List(
        if (config.enableSparks) Some(SparkIgnition[F](config.sparkProbability)) else None,
        Some(NeighborIgnition[F]()),
        if (config.enableEmbers) Some(EmberIgnition[F](config.emberDistance)) else None
      ).flatten,
      
      burningRules = List(
        IntensityEvolution[F](),
        FuelConsumption[F](),
        HeatGeneration[F]()
      ),
      
      extinctionRules = List(
        FuelDepletion[F](),
        MoistureSuppression[F](),
        TemperatureDecay[F]()
      ),
      
      recoveryRules = if (config.enableRegrowth) List(
        NaturalRegrowth[F](config.regrowthRate),
        SeasonalGrowth[F]()
      ) else List.empty,
      
      interventionRules = List(
        if (config.interventionRules.enableWaterDrops) Some(WaterDropping[F]()) else None,
        if (config.interventionRules.enableFirebreaks) 
          Some(FirebreakEffect[F](config.interventionRules.firebreakPositions)) 
        else None
      ).flatten
    )
  }
}

/**
 * Configuration for rules
 */
case class RuleConfig(
  enableSparks: Boolean = true,
  sparkProbability: Double = 0.0001,
  enableEmbers: Boolean = true,
  emberDistance: Int = 5,
  enableRegrowth: Boolean = true,
  regrowthRate: Double = 0.01,
  interventionRules: InterventionConfig = InterventionConfig()
)

case class InterventionConfig(
  enableWaterDrops: Boolean = false,
  enableFirebreaks: Boolean = false,
  firebreakPositions: List[Position] = List.empty
)