package simulation.rules

import models._
import simulation.SimulationState
import cats.Monad
import cats.implicits._

/**
 * Base trait for all simulation rules
 * Rules are pure functions that transform cells based on their state and environment
 */
trait Rule[F[_]] {
  /**
   * Apply this rule to a cell
   * @param cell The cell to potentially transform
   * @param neighbors The cell's neighbors
   * @param state The current simulation state
   * @return The potentially transformed cell
   */
  def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell]
  
  /**
   * Check if this rule is applicable to the given cell
   * @param cell The cell to check
   * @param state The current simulation state
   * @return True if the rule should be applied
   */
  def isApplicable(cell: Cell, state: SimulationState): Boolean
  
  /**
   * Rule name for debugging and configuration
   */
  def name: String
}

/**
 * Utility for composing rules
 */
object Rule {
  /**
   * Create a rule that applies multiple rules in sequence
   */
  def sequence[F[_]: Monad](rules: List[Rule[F]]): Rule[F] = new Rule[F] {
    override def name: String = s"Sequence(${rules.map(_.name).mkString(", ")})"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      rules.exists(_.isApplicable(cell, state))
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] =
      rules.foldLeftM(cell) { (currentCell, rule) =>
        if (rule.isApplicable(currentCell, state))
          rule.apply(currentCell, neighbors, state)
        else
          currentCell.pure[F]
      }
  }
  
  /**
   * Create a conditional rule
   */
  def conditional[F[_]: Monad](
    condition: (Cell, SimulationState) => Boolean,
    rule: Rule[F]
  ): Rule[F] = new Rule[F] {
    override def name: String = s"Conditional(${rule.name})"
    
    override def isApplicable(cell: Cell, state: SimulationState): Boolean =
      condition(cell, state) && rule.isApplicable(cell, state)
    
    override def apply(cell: Cell, neighbors: List[Cell], state: SimulationState): F[Cell] =
      if (condition(cell, state))
        rule.apply(cell, neighbors, state)
      else
        cell.pure[F]
  }
  
  /**
   * Create a rule that only applies to cells in a specific state
   */
  def forState[F[_]: Monad](targetState: CellState, rule: Rule[F]): Rule[F] =
    conditional[F](
      (cell, _) => cell.state == targetState,
      rule
    )
}