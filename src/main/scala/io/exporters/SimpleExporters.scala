package io.exporters

import io._
import data._
import models._
import simulation._
import cats.effect.IO
import java.nio.file.{Files, Path, Paths}
import java.io.PrintWriter
import scala.collection.mutable

/**
 * Simple CSV exporter for simulation results
 */
object SimpleCSVExporter {
  
  def exportSimulationResults(
    states: List[SimulationState],
    outputPath: Path
  ): IO[Unit] = IO {
    Files.createDirectories(outputPath.getParent)
    
    val writer = new PrintWriter(outputPath.toFile)
    try {
      // Write header
      writer.println("time,active_fires,burnt_area,largest_cluster,tree_density,percolation_indicator")
      
      // Write data for each state
      states.foreach { state =>
        val metrics = state.metrics
        writer.println(
          f"${state.elapsedTime}%.2f,${metrics.activeFires},${metrics.totalBurntArea}," +
          f"${metrics.largestFireClusterSize},${metrics.treeDensity}%.3f," +
          f"${metrics.percolationIndicator}%.3f"
        )
      }
    } finally {
      writer.close()
    }
  }
  
  def exportPhaseData(
    phaseData: List[(Double, analysis.Phase, Map[String, Double])],
    outputPath: Path
  ): IO[Unit] = IO {
    Files.createDirectories(outputPath.getParent)
    
    val writer = new PrintWriter(outputPath.toFile)
    try {
      writer.println("parameter_value,phase,burnt_fraction,percolation")
      
      phaseData.foreach { case (value, phase, orderParams) =>
        val burntFraction = orderParams.getOrElse("burntFraction", 0.0)
        val percolation = orderParams.getOrElse("percolationIndicator", 0.0)
        writer.println(f"$value%.3f,$phase,$burntFraction%.3f,$percolation%.3f")
      }
    } finally {
      writer.close()
    }
  }
  
  def exportGridSnapshot(
    grid: Grid,
    outputPath: Path
  ): IO[Unit] = IO {
    Files.createDirectories(outputPath.getParent)
    
    val writer = new PrintWriter(outputPath.toFile)
    try {
      writer.println("x,y,state,elevation,vegetation,moisture,temperature")
      
      for {
        y <- 0 until grid.height
        x <- 0 until grid.width
      } {
        val cell = grid(x, y)
        writer.println(
          s"$x,$y,${cell.state},${cell.elevation},${cell.vegetationType}," +
          f"${cell.moisture}%.3f,${cell.temperature}%.1f"
        )
      }
    } finally {
      writer.close()
    }
  }
  
  def exportClimateScenarios(
    scenarios: Map[(io.swiss.SwissDataModels.ClimateScenario, Int), Climate],
    outputPath: Path
  ): IO[Unit] = IO {
    Files.createDirectories(outputPath.getParent)
    
    val writer = new PrintWriter(outputPath.toFile)
    try {
      writer.println("scenario,year,humidity,wind_speed,wind_direction")
      
      scenarios.foreach { case ((scenario, year), climate) =>
        val scenarioName = scenario match {
          case io.swiss.SwissDataModels.RCP26 => "RCP2.6"
          case io.swiss.SwissDataModels.RCP45 => "RCP4.5"
          case io.swiss.SwissDataModels.RCP85 => "RCP8.5"
        }
        writer.println(
          f"$scenarioName,$year,${climate.humidity}%.3f," +
          f"${climate.wind.speed}%.1f,${climate.wind.direction}%.0f"
        )
      }
    } finally {
      writer.close()
    }
  }
}