package simulation

import models._
import scala.util.Random

/**
 * Logic for updating individual cells based on fire dynamics
 */
object CellUpdateLogic {
  
  /**
   * Update a single cell based on its current state and environment
   */
  def updateCell(
    cell: Cell,
    neighbors: List[Cell],
    climate: Climate,
    terrain: Terrain,
    dt: Double,
    params: FireDynamics.FireDynamicsParameters = FireDynamics.FireDynamicsParameters(),
    random: Random = new Random()
  ): (Cell, List[FireEvent]) = {
    
    val events = scala.collection.mutable.ListBuffer[FireEvent]()
    
    val updatedCell = cell.state match {
      case Empty => 
        // Empty cells remain empty
        cell
        
      case Tree =>
        // Trees can ignite based on fire dynamics
        val burningNeighbors = neighbors.count(_.state == Burning)
        
        if (burningNeighbors > 0) {
          val ignitionProb = FireDynamics.calculateIgnitionProbability(
            cell, climate, terrain, burningNeighbors, params
          )
          
          if (random.nextDouble() < ignitionProb * dt) {
            events += IgnitionEvent(0.0, cell.position) // Time will be set by engine
            cell.ignite
          } else {
            // Pre-heat the cell from nearby fires
            val heatTransfer = FireDynamics.calculateHeatTransfer(
              cell, neighbors.filter(_.state == Burning), terrain, climate, params
            )
            val preHeatedCell = FireDynamics.preHeatCell(cell, heatTransfer, dt)
            
            // Update moisture
            val temperature = climate.temperatureAtElevation(cell.elevation)
            val newMoisture = FireDynamics.updateMoisture(
              preHeatedCell.moisture, temperature, climate.humidity, climate.precipitation, dt, params
            )
            
            preHeatedCell.copy(moisture = newMoisture)
          }
        } else {
          // No fire nearby, just update moisture
          val temperature = climate.temperatureAtElevation(cell.elevation)
          val newMoisture = FireDynamics.updateMoisture(
            cell.moisture, temperature, climate.humidity, climate.precipitation, dt, params
          )
          cell.copy(moisture = newMoisture, temperature = temperature)
        }
        
      case Burning =>
        // Burning cells can extinguish or burn out
        val temperature = climate.temperatureAtElevation(cell.elevation)
        
        // Check for extinction due to environmental factors
        val extinctionProb = FireDynamics.calculateExtinctionProbability(
          cell, climate, climate.precipitation
        )
        
        if (random.nextDouble() < extinctionProb * dt) {
          events += ExtinctionEvent(0.0, cell.position)
          // Extinguished cells become trees again (partially burnt)
          cell.copy(state = Tree, moisture = 0.8, temperature = temperature)
        } else {
          // Check for fuel depletion
          val burnDuration = dt * 10 // Approximate burn duration tracking
          if (FireDynamics.isFuelDepleted(cell, burnDuration)) {
            events += BurnoutEvent(0.0, cell.position)
            cell.burnOut
          } else {
            // Continue burning, update temperature
            cell.copy(temperature = cell.temperature + 50.0 * dt)
          }
        }
        
      case Burnt =>
        // Burnt cells slowly recover moisture and cool down
        val temperature = climate.temperatureAtElevation(cell.elevation)
        val newMoisture = FireDynamics.updateMoisture(
          cell.moisture, temperature, climate.humidity, climate.precipitation * 2.0, dt, params
        )
        val coolingRate = 0.1
        val newTemp = cell.temperature - (cell.temperature - temperature) * coolingRate * dt
        
        cell.copy(moisture = newMoisture, temperature = newTemp)
    }
    
    (updatedCell, events.toList)
  }
  
  /**
   * Update multiple cells in parallel (for synchronous update)
   */
  def updateCells(
    cells: Seq[(Cell, List[Cell])], // (cell, neighbors) pairs
    climate: Climate,
    terrain: Terrain,
    dt: Double,
    params: FireDynamics.FireDynamicsParameters = FireDynamics.FireDynamicsParameters(),
    random: Random = new Random()
  ): Seq[(Cell, List[FireEvent])] = {
    cells.map { case (cell, neighbors) =>
      updateCell(cell, neighbors, climate, terrain, dt, params, random)
    }
  }
  
  /**
   * Check if a cell is active (needs updating)
   */
  def isActiveCell(cell: Cell): Boolean = cell.state match {
    case Burning => true
    case _ => false
  }
  
  /**
   * Check if a cell is near active cells (optimization)
   */
  def isNearActiveCell(cell: Cell, neighbors: List[Cell]): Boolean = {
    cell.state == Tree && neighbors.exists(_.state == Burning)
  }
}