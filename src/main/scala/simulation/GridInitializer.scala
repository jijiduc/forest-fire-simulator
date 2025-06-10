package simulation

import models._
import scala.util.Random

object GridInitializer {
  
  def initializeGrid(terrain: Terrain, climate: Climate, seed: Long = System.currentTimeMillis): Grid = {
    val random = new Random(seed)
    val cells = Vector.tabulate(terrain.height, terrain.width) { (y, x) =>
      createCell(x, y, terrain, climate, random)
    }
    Grid(cells, terrain.width, terrain.height)
  }
  
  private def createCell(x: Int, y: Int, terrain: Terrain, climate: Climate, random: Random): Cell = {
    val position = Position(x, y)
    val elevation = terrain.elevationAt(x, y)
    val vegetationType = terrain.vegetationTypeAt(x, y)
    val temperature = climate.temperatureAtElevation(elevation)
    
    val moisture = calculateMoisture(elevation, terrain.aspectAt(x, y), climate, random)
    
    val state = determineInitialState(elevation, vegetationType, climate, random)
    
    Cell(position, state, elevation, vegetationType, moisture, temperature)
  }
  
  private def calculateMoisture(elevation: Double, aspect: Double, climate: Climate, random: Random): Double = {
    val baseMoisture = climate.humidity
    val aspectFactor = if (math.abs(aspect) < math.Pi / 2) 0.9 else 1.1
    val elevationFactor = 1.0 + (elevation / 5000.0)
    val randomFactor = 0.9 + random.nextDouble() * 0.2
    
    (baseMoisture * aspectFactor * elevationFactor * randomFactor).min(1.0).max(0.0)
  }
  
  private def determineInitialState(elevation: Double, vegetationType: VegetationType, climate: Climate, random: Random): CellState = {
    if (elevation > climate.season.snowLineElevation) {
      Empty
    } else {
      vegetationType match {
        case VegetationType.Barren => Empty
        case VegetationType.Water => Empty
        case VegetationType.Urban => Empty
        case VegetationType.Grassland => if (random.nextDouble() < 0.3) Tree else Empty
        case VegetationType.Shrubland => if (random.nextDouble() < 0.5) Tree else Empty
        case VegetationType.SparseForest => if (random.nextDouble() < 0.7) Tree else Empty
        case VegetationType.DenseForest => if (random.nextDouble() < 0.9) Tree else Empty
      }
    }
  }
}