package io.exporters

import cats.effect._
import cats.implicits._
import fs2._
import models._
import simulation._
import java.nio.file._
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer

trait VideoDataExporter[F[_]] {
  def exportFrame(state: SimulationState, frameNumber: Int): F[Unit]
  def exportMetadata(config: SimulationConfig, metrics: List[SimulationMetrics]): F[Unit]
  def exportElevationData(grid: Grid): F[Unit]
}

class CSVVideoDataExporter[F[_]: Sync](outputDir: Path) extends VideoDataExporter[F] {
  
  // Ensure output directory exists
  private def ensureDirectory: F[Unit] = Sync[F].delay {
    Files.createDirectories(outputDir)
    Files.createDirectories(outputDir.resolve("frames"))
    Files.createDirectories(outputDir.resolve("metadata"))
  }
  
  override def exportFrame(state: SimulationState, frameNumber: Int): F[Unit] = for {
    _ <- ensureDirectory
    framePath = outputDir.resolve(f"frames/frame_$frameNumber%06d.csv")
    _ <- exportGrid(state.grid, framePath)
    metricsPath = outputDir.resolve(f"frames/metrics_$frameNumber%06d.csv")
    _ <- exportFrameMetrics(state, frameNumber, metricsPath)
  } yield ()
  
  override def exportMetadata(config: SimulationConfig, metrics: List[SimulationMetrics]): F[Unit] = for {
    _ <- ensureDirectory
    configPath = outputDir.resolve("metadata/config.csv")
    _ <- exportConfig(config, configPath)
    timelinePath = outputDir.resolve("metadata/timeline.csv")
    _ <- exportTimeline(metrics, timelinePath)
  } yield ()
  
  override def exportElevationData(grid: Grid): F[Unit] = for {
    _ <- ensureDirectory
    elevationPath = outputDir.resolve("metadata/elevation.csv")
    _ <- exportElevation(grid, elevationPath)
  } yield ()
  
  private def exportGrid(grid: Grid, path: Path): F[Unit] = Sync[F].delay {
    val writer = new PrintWriter(path.toFile)
    try {
      writer.println("x,y,state,vegetation,moisture,temperature,elevation")
      for {
        y <- 0 until grid.height
        x <- 0 until grid.width
      } {
        val cell = grid(x, y)
        val stateStr = cell.state match {
          case Empty => "empty"
          case Tree => "tree"
          case Burning => "burning_0.5"
          case Burnt => "burnt"
        }
        val veg = cell.vegetationType.getClass.getSimpleName.replace("$", "")
        val moisture = f"${cell.moisture}%.3f"
        val temp = f"${cell.temperature}%.1f"
        val elev = f"${cell.elevation}%.1f"
        writer.println(s"$x,$y,$stateStr,$veg,$moisture,$temp,$elev")
      }
    } finally {
      writer.close()
    }
  }
  
  private def exportFrameMetrics(state: SimulationState, frameNumber: Int, path: Path): F[Unit] = Sync[F].delay {
    val writer = new PrintWriter(path.toFile)
    try {
      writer.println("frame,time,total_cells,tree_cells,burning_cells,burnt_cells,empty_cells")
      val total = state.grid.width * state.grid.height
      val allCells = state.grid.cellsWithPosition.map(_._3)
      val trees = allCells.count(_.state == Tree)
      val burning = allCells.count(_.state == Burning)
      val burnt = allCells.count(_.state == Burnt)
      val empty = allCells.count(_.state == Empty)
      
      writer.println(s"$frameNumber,${state.elapsedTime},$total,$trees,$burning,$burnt,$empty")
    } finally {
      writer.close()
    }
  }
  
  private def exportConfig(config: SimulationConfig, path: Path): F[Unit] = Sync[F].delay {
    val writer = new PrintWriter(path.toFile)
    try {
      writer.println("parameter,value")
      writer.println(s"maxSteps,${config.maxSteps}")
      writer.println(s"maxTime,${config.maxTime}")
      writer.println(s"minTimeStep,${config.minTimeStep}")
      writer.println(s"maxTimeStep,${config.maxTimeStep}")
      writer.println(s"adaptiveTimeStep,${config.adaptiveTimeStep}")
      writer.println(s"boundaryCondition,${config.boundaryCondition}")
      writer.println(s"updateStrategy,${config.updateStrategy}")
      writer.println(s"parallelism,${config.parallelism}")
    } finally {
      writer.close()
    }
  }
  
  private def exportTimeline(metrics: List[SimulationMetrics], path: Path): F[Unit] = Sync[F].delay {
    val writer = new PrintWriter(path.toFile)
    try {
      writer.println("time,burntFraction,activeFires,largestClusterSize,percolationIndicator,treeDensity")
      metrics.zipWithIndex.foreach { case (m, idx) =>
        val time = idx * 0.1  // Assuming time step
        val burntFraction = m.totalBurntArea.toDouble / (100 * 100)  // Assuming 100x100 grid
        writer.println(s"$time,$burntFraction,${m.activeFires},${m.largestFireClusterSize},${m.percolationIndicator},${m.treeDensity}")
      }
    } finally {
      writer.close()
    }
  }
  
  private def exportElevation(grid: Grid, path: Path): F[Unit] = Sync[F].delay {
    val writer = new PrintWriter(path.toFile)
    try {
      writer.println("x,y,elevation")
      for {
        y <- 0 until grid.height
        x <- 0 until grid.width
      } {
        val cell = grid(x, y)
        val elev = f"${cell.elevation}%.1f"
        writer.println(s"$x,$y,$elev")
      }
    } finally {
      writer.close()
    }
  }
}

// Extension to SimulationEngine for video export
object VideoExportEngine {
  def runWithVideoExport[F[_]: Async](
    engine: SimulationEngine[F],
    exporter: VideoDataExporter[F],
    initial: SimulationState,
    steps: Int,
    config: SimulationConfig,
    exportInterval: Int = 10
  ): Stream[F, SimulationState] = {
    
    // Export initial state
    val exportInitial = Stream.eval(exporter.exportFrame(initial, 0))
    
    // Run simulation with periodic exports
    val simulation = engine.run(initial, steps, config)
      .zipWithIndex
      .evalMap { case (state, index) =>
        val frameNumber = index.toInt + 1
        if (frameNumber % exportInterval == 0) {
          exporter.exportFrame(state, frameNumber / exportInterval).as(state)
        } else {
          Async[F].pure(state)
        }
      }
    
    exportInitial >> simulation
  }
}