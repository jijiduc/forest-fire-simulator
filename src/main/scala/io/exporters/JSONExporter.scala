package io.exporters

import io._
import data._
import models._
import simulation._
import cats.effect.IO
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.io.PrintWriter
import scala.collection.mutable

/**
 * JSON exporter for web visualization
 * Exports simulation states in a format optimized for web viewing
 */
object JSONExporter {
  
  case class CellData(
    x: Int,
    y: Int,
    state: String,
    elevation: Double,
    vegetation: String,
    moisture: Double,
    temperature: Double,
    fireIntensity: Double
  )
  
  case class FrameData(
    time: Double,
    metrics: MetricsData,
    cells: List[CellData],
    fullFrame: Boolean = false
  )
  
  case class MetricsData(
    activeFires: Int,
    burntArea: Int,
    largestCluster: Int,
    treeDensity: Double,
    percolationIndicator: Double
  )
  
  case class SimulationMetadata(
    width: Int,
    height: Int,
    timesteps: Int,
    deltaTime: Double,
    parameters: Map[String, String],
    terrain: Option[TerrainInfo] = None
  )
  
  case class TerrainInfo(
    minElevation: Double,
    maxElevation: Double,
    hasSwissData: Boolean
  )
  
  case class SimulationExport(
    metadata: SimulationMetadata,
    frames: List[FrameData]
  )
  
  /**
   * Export full simulation timeline with delta compression
   */
  def exportSimulation(
    states: List[SimulationState],
    outputPath: Path,
    deltaCompression: Boolean = true,
    frameInterval: Int = 1
  ): IO[Unit] = IO {
    if (states.isEmpty) {
      throw new IllegalArgumentException("No states to export")
    }
    
    val firstState = states.head
    val grid = firstState.grid
    
    // Calculate terrain info
    val elevations = for {
      y <- 0 until grid.height
      x <- 0 until grid.width
    } yield grid(x, y).elevation
    
    val terrainInfo = TerrainInfo(
      minElevation = elevations.min,
      maxElevation = elevations.max,
      hasSwissData = elevations.max > 1000.0 // Simple heuristic
    )
    
    // Create metadata
    val metadata = SimulationMetadata(
      width = grid.width,
      height = grid.height,
      timesteps = states.length,
      deltaTime = if (states.length > 1) states(1).elapsedTime - states(0).elapsedTime else 1.0,
      parameters = Map(
        "windSpeed" -> firstState.climate.wind.speed.toString,
        "windDirection" -> firstState.climate.wind.direction.toString,
        "humidity" -> firstState.climate.humidity.toString,
        "precipitation" -> firstState.climate.precipitation.toString,
        "season" -> firstState.climate.season.toString
      ),
      terrain = Some(terrainInfo)
    )
    
    // Convert states to frames
    var previousCells: Map[(Int, Int), CellData] = Map.empty
    val frames = states.zipWithIndex.filter { case (_, idx) => 
      idx % frameInterval == 0 
    }.map { case (state, idx) =>
      val currentCells = extractCellData(state.grid)
      
      val frameCells = if (deltaCompression && idx > 0) {
        // Only include changed cells
        currentCells.filter { cell =>
          val key = (cell.x, cell.y)
          previousCells.get(key).map(_ != cell).getOrElse(true)
        }
      } else {
        currentCells
      }
      
      // Update previous cells
      currentCells.foreach { cell =>
        previousCells += ((cell.x, cell.y) -> cell)
      }
      
      FrameData(
        time = state.elapsedTime,
        metrics = MetricsData(
          activeFires = state.metrics.activeFires,
          burntArea = state.metrics.totalBurntArea,
          largestCluster = state.metrics.largestFireClusterSize,
          treeDensity = state.metrics.treeDensity,
          percolationIndicator = state.metrics.percolationIndicator
        ),
        cells = frameCells,
        fullFrame = idx == 0
      )
    }
    
    // Write JSON file
    Files.createDirectories(outputPath.getParent)
    val writer = new PrintWriter(outputPath.toFile)
    try {
      val exportData = SimulationExport(metadata, frames)
      writer.write(exportData.asJson.spaces2)
    } finally {
      writer.close()
    }
  }
  
  /**
   * Export a single grid snapshot
   */
  def exportGridSnapshot(
    grid: Grid,
    time: Double,
    metrics: SimulationMetrics,
    outputPath: Path
  ): IO[Unit] = IO {
    val metadata = SimulationMetadata(
      width = grid.width,
      height = grid.height,
      timesteps = 1,
      deltaTime = 0.0,
      parameters = Map.empty
    )
    
    val frame = FrameData(
      time = time,
      metrics = MetricsData(
        activeFires = metrics.activeFires,
        burntArea = metrics.totalBurntArea,
        largestCluster = metrics.largestFireClusterSize,
        treeDensity = metrics.treeDensity,
        percolationIndicator = metrics.percolationIndicator
      ),
      cells = extractCellData(grid),
      fullFrame = true
    )
    
    val exportData = SimulationExport(metadata, List(frame))
    
    Files.createDirectories(outputPath.getParent)
    val writer = new PrintWriter(outputPath.toFile)
    try {
      writer.write(exportData.asJson.spaces2)
    } finally {
      writer.close()
    }
  }
  
  private def extractCellData(grid: Grid): List[CellData] = {
    (for {
      y <- 0 until grid.height
      x <- 0 until grid.width
    } yield {
      val cell = grid(x, y)
      CellData(
        x = x,
        y = y,
        state = cell.state.toString,
        elevation = cell.elevation,
        vegetation = cell.vegetationType.toString,
        moisture = cell.moisture,
        temperature = cell.temperature,
        fireIntensity = cell.state match {
          case Burning => 0.8 // Default intensity for burning cells
          case _ => 0.0
        }
      )
    }).toList
  }
  
  // Implicit encoder for Cell types
  implicit val cellStateEncoder: Encoder[CellState] = Encoder.instance {
    case Empty => Json.fromString("Empty")
    case Tree => Json.fromString("Tree")
    case Burning => Json.fromString("Burning")
    case Burnt => Json.fromString("Burnt")
  }
  
  implicit val vegetationEncoder: Encoder[VegetationType] = Encoder.instance { veg =>
    Json.fromString(veg.toString)
  }
}