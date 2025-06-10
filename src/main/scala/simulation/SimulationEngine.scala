package simulation

import cats.effect._
import fs2.Stream

/**
 * Main trait for simulation engines
 * F[_] represents the effect type (IO, Future, etc.)
 */
trait SimulationEngine[F[_]] {
  /**
   * Execute a single simulation step
   */
  def step(state: SimulationState, config: SimulationConfig): F[SimulationState]
  
  /**
   * Run simulation for a fixed number of steps
   */
  def run(initial: SimulationState, steps: Int, config: SimulationConfig): Stream[F, SimulationState]
  
  /**
   * Run simulation until a condition is met
   */
  def runUntil(
    initial: SimulationState, 
    condition: SimulationState => Boolean,
    config: SimulationConfig
  ): Stream[F, SimulationState]
  
  /**
   * Run simulation with adaptive time stepping until maxTime
   */
  def runAdaptive(
    initial: SimulationState, 
    maxTime: Double,
    config: SimulationConfig
  ): Stream[F, SimulationState]
}