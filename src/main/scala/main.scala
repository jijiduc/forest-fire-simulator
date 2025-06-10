import models._
import terrain.TerrainGenerator
import simulation.GridInitializer

object ForestFireSimulator extends App {
  println("Forest Fire Simulator - Alpine Ecosystem")
  println("=========================================")
  
  val width = 100
  val height = 100
  
  println(s"Generating ${width}x${height} alpine terrain...")
  val terrain = TerrainGenerator.generateTerrain(width, height)
  
  val climate = Climate(
    season = Summer,
    wind = Wind(direction = math.Pi / 4, speed = 5.0),
    humidity = 0.4,
    precipitation = 0.0
  )
  
  println(s"Initializing grid for ${climate.season} season...")
  val grid = GridInitializer.initializeGrid(terrain, climate)
  
  val stats = grid.cellsWithPosition.groupBy(_._3.state).map { case (state, cells) =>
    state -> cells.length
  }
  
  println("\nInitial cell distribution:")
  stats.foreach { case (state, count) =>
    println(s"  $state: $count cells")
  }
  
  val elevationStats = grid.cellsWithPosition.groupBy { case (_, _, cell) =>
    (cell.elevation / 500).toInt * 500
  }.map { case (elevation, cells) =>
    elevation -> cells.length
  }.toSeq.sortBy(_._1)
  
  println("\nElevation distribution:")
  elevationStats.foreach { case (elevation, count) =>
    println(f"  ${elevation}%4d - ${elevation + 500}%4d m: $count%5d cells")
  }
  
  println("\nPhase 1 implementation complete!")
}