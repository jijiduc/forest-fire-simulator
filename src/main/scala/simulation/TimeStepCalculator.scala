package simulation

import models._
import scala.math._

/**
 * Calculates adaptive time steps for the simulation
 */
trait TimeStepCalculator {
  def calculateDt(state: SimulationState, config: SimulationConfig): Double
}

/**
 * Simple fixed time step calculator
 */
object FixedTimeStep extends TimeStepCalculator {
  def calculateDt(state: SimulationState, config: SimulationConfig): Double = {
    config.minTimeStep
  }
}

/**
 * Adaptive time step based on fire activity and CFL condition
 */
object AdaptiveTimeStep extends TimeStepCalculator {
  
  def calculateDt(state: SimulationState, config: SimulationConfig): Double = {
    if (!config.adaptiveTimeStep) {
      return config.minTimeStep
    }
    
    // Find maximum fire spread rate in the grid
    val maxSpreadRate = findMaxSpreadRate(state)
    
    // CFL condition: dt < dx / v_max
    // where dx is cell size (assumed 1.0) and v_max is max spread rate
    val cflDt = if (maxSpreadRate > 0) 0.5 / maxSpreadRate else config.maxTimeStep
    
    // Consider fire activity level
    val activityDt = calculateActivityBasedDt(state)
    
    // Take minimum of all constraints
    val dt = min(cflDt, activityDt)
    
    // Clamp to configured bounds
    max(config.minTimeStep, min(config.maxTimeStep, dt))
  }
  
  private def findMaxSpreadRate(state: SimulationState): Double = {
    val burningCells = state.grid.cells.flatten.filter(_.state == Burning)
    
    if (burningCells.isEmpty) {
      0.0
    } else {
      burningCells.map { cell =>
        FireDynamics.calculateFireSpreadRate(
          cell, 
          state.climate, 
          state.terrain, 
          state.climate.wind.speed
        )
      }.max
    }
  }
  
  private def calculateActivityBasedDt(state: SimulationState): Double = {
    val metrics = state.metrics
    
    // More active fires require smaller time steps
    if (metrics.activeFires == 0) {
      1.0 // Large time step when no fires
    } else if (metrics.activeFires < 10) {
      0.5 // Medium time step for few fires
    } else if (metrics.activeFires < 50) {
      0.1 // Small time step for moderate fires
    } else {
      0.05 // Very small time step for many fires
    }
  }
}

/**
 * Time step calculator that adjusts based on rate of change
 */
object ChangeRateTimeStep extends TimeStepCalculator {
  
  private var previousMetrics: Option[SimulationMetrics] = None
  
  def calculateDt(state: SimulationState, config: SimulationConfig): Double = {
    val currentMetrics = state.metrics
    
    val dt = previousMetrics match {
      case None =>
        // First step, use default
        config.minTimeStep
        
      case Some(prev) =>
        // Calculate rate of change
        val fireChange = abs(currentMetrics.activeFires - prev.activeFires)
        val areaChange = abs(currentMetrics.totalBurntArea - prev.totalBurntArea)
        
        // Normalize changes
        val normalizedChange = fireChange / max(1, prev.activeFires) + 
                              areaChange / max(1, prev.totalBurntArea)
        
        // Adjust dt inversely to rate of change
        if (normalizedChange > 0.5) {
          state.timeStep * 0.5 // Decrease dt for rapid changes
        } else if (normalizedChange < 0.1) {
          state.timeStep * 1.5 // Increase dt for slow changes
        } else {
          state.timeStep // Keep current dt
        }
    }
    
    previousMetrics = Some(currentMetrics)
    
    // Clamp to bounds
    max(config.minTimeStep, min(config.maxTimeStep, dt))
  }
}