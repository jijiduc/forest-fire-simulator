package simulation.rules

import models._
import simulation._
import cats.Monad
import cats.implicits._

/**
 * Rules for natural intervention events (no human management)
 * These represent natural phenomena that can affect fire behavior
 */
object InterventionRules {
  
  /**
   * Natural water features (rivers, lakes) that block fire
   * This is a placeholder for future terrain-based water features
   */
  case class NaturalWaterBarrier[F[_]: Monad]() extends Rule[F] {
    override def name: String = "NaturalWaterBarrier"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean = false
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = 
      cell.pure[F]
  }
  
  /**
   * Placeholder for water dropping intervention
   * Not implemented as per requirements
   */
  case class WaterDropping[F[_]: Monad]() extends Rule[F] {
    override def name: String = "WaterDropping"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean = false
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = 
      cell.pure[F]
  }
  
  /**
   * Placeholder for firebreak intervention
   * Not implemented as per requirements
   */
  case class FirebreakEffect[F[_]: Monad](positions: List[Position]) extends Rule[F] {
    override def name: String = "FirebreakEffect"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean = false
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] = 
      cell.pure[F]
  }
}