package app

import cats.effect._
import cats.effect.std.Console
import cats.implicits._
import cli._
import io.exporters._
import simulation._
import models._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import java.nio.file.{Files, Path}
import scala.io.Source
import io.circe._
import io.circe.parser._

class ExportRunner[F[_]: Async: Console](
  config: AppConfig,
  progressReporter: ProgressReporter[F]
) {
  
  implicit def logger: Logger[F] = Slf4jLogger.getLogger[F]
  
  def run(command: ExportCommand): F[ExitCode] = {
    for {
      _ <- logger.info(s"Exporting ${command.inputFile} to ${command.format}")
      _ <- Console[F].println(s"\n📤 Export: ${command.format.toUpperCase} format")
      _ <- Console[F].println("=" * 60)
      
      // Check input file exists
      _ <- checkInputFile(command.inputFile)
      
      // Determine input format
      inputFormat <- detectInputFormat(command.inputFile)
      _ <- Console[F].println(s"\n📂 Input format detected: $inputFormat")
      
      // Load data
      _ <- Console[F].println(s"📖 Loading data from ${command.inputFile}...")
      data <- loadData(command.inputFile, inputFormat)
      
      // Export to requested format
      _ <- Console[F].println(s"\n✍️  Converting to ${command.format}...")
      _ <- progressReporter.start(100, s"Exporting to ${command.format}")
      
      _ <- command.format.toLowerCase match {
        case ExportFormats.CSV => exportToCSV(data, command.output)
        case ExportFormats.JSON => exportToJSON(data, command.output)
        case ExportFormats.NetCDF => exportToNetCDF(data, command.output)
        case _ => Sync[F].raiseError(new IllegalArgumentException(s"Unsupported format: ${command.format}"))
      }
      
      _ <- progressReporter.finish("Export complete!")
      
      // Show file info
      fileSize <- getFileSize(command.output)
      _ <- Console[F].println(s"\n✅ Export successful!")
      _ <- Console[F].println(s"   Output file: ${command.output}")
      _ <- Console[F].println(s"   File size: ${formatFileSize(fileSize)}")
      
    } yield ExitCode.Success
  }.handleErrorWith { error =>
    for {
      _ <- logger.error(error)("Export failed")
      _ <- Console[F].errorln(ConsoleFormatter.formatError(error))
    } yield ExitCode.Error
  }
  
  private def checkInputFile(path: Path): F[Unit] = 
    Sync[F].delay {
      if (!Files.exists(path)) {
        throw new IllegalArgumentException(s"Input file does not exist: $path")
      }
    }
  
  private def detectInputFormat(path: Path): F[String] = Sync[F].delay {
    val filename = path.getFileName.toString.toLowerCase
    if (filename.endsWith(".csv")) "csv"
    else if (filename.endsWith(".json")) "json"
    else "unknown"
  }
  
  private def loadData(path: Path, format: String): F[ExportData] = format match {
    case "csv" => loadCSVData(path)
    case "json" => loadJSONData(path)
    case _ => Sync[F].raiseError(new IllegalArgumentException(s"Cannot load format: $format"))
  }
  
  private def loadCSVData(path: Path): F[ExportData] = Sync[F].delay {
    val source = Source.fromFile(path.toFile)
    try {
      val lines = source.getLines().toList
      if (lines.isEmpty) throw new RuntimeException("Empty CSV file")
      
      val headers = lines.head.split(",").map(_.trim)
      val data = lines.tail.map(_.split(",").map(_.trim))
      
      // Detect data type based on headers
      if (headers.contains("timeStep") && headers.contains("activeFires")) {
        // Time series data
        ExportData.TimeSeries(
          headers = headers.toList,
          rows = data.map(_.toList).toList
        )
      } else if (headers.contains("x") && headers.contains("y") && headers.contains("state")) {
        // Grid snapshot
        ExportData.GridSnapshot(
          headers = headers.toList,
          rows = data.map(_.toList).toList
        )
      } else {
        // Generic tabular data
        ExportData.Tabular(
          headers = headers.toList,
          rows = data.map(_.toList).toList
        )
      }
    } finally {
      source.close()
    }
  }
  
  private def loadJSONData(path: Path): F[ExportData] = Sync[F].delay {
    val content = new String(Files.readAllBytes(path), "UTF-8")
    parse(content) match {
      case Right(json) =>
        // Try to detect simulation data structure
        json.hcursor.downField("metadata").focus match {
          case Some(_) =>
            // Simulation data
            ExportData.SimulationJSON(json)
          case None =>
            // Generic JSON
            ExportData.GenericJSON(json)
        }
      case Left(error) =>
        throw new RuntimeException(s"Invalid JSON: ${error.message}")
    }
  }
  
  private def exportToCSV(data: ExportData, output: Path): F[Unit] = data match {
    case ExportData.TimeSeries(headers, rows) =>
      writeCSV(headers, rows, output) *> progressReporter.update(100)
      
    case ExportData.GridSnapshot(headers, rows) =>
      writeCSV(headers, rows, output) *> progressReporter.update(100)
      
    case ExportData.Tabular(headers, rows) =>
      writeCSV(headers, rows, output) *> progressReporter.update(100)
      
    case ExportData.SimulationJSON(json) =>
      // Convert simulation JSON to CSV time series
      for {
        _ <- progressReporter.update(25)
        timeSeries <- extractTimeSeriesFromJSON(json)
        _ <- progressReporter.update(75)
        _ <- writeCSV(timeSeries.headers, timeSeries.rows, output)
        _ <- progressReporter.update(100)
      } yield ()
      
    case ExportData.GenericJSON(json) =>
      // Flatten JSON to CSV
      for {
        _ <- progressReporter.update(50)
        tabular <- flattenJSON(json)
        _ <- writeCSV(tabular.headers, tabular.rows, output)
        _ <- progressReporter.update(100)
      } yield ()
  }
  
  private def exportToJSON(data: ExportData, output: Path): F[Unit] = data match {
    case ExportData.TimeSeries(headers, rows) =>
      val json = Json.obj(
        "type" -> Json.fromString("timeSeries"),
        "headers" -> Json.fromValues(headers.map(Json.fromString)),
        "data" -> Json.fromValues(rows.map(row =>
          Json.obj(headers.zip(row).map { case (h, v) =>
            h -> Json.fromString(v)
          }: _*)
        ))
      )
      writeJSON(json, output) *> progressReporter.update(100)
      
    case ExportData.GridSnapshot(headers, rows) =>
      val json = Json.obj(
        "type" -> Json.fromString("gridSnapshot"),
        "headers" -> Json.fromValues(headers.map(Json.fromString)),
        "data" -> Json.fromValues(rows.map(row =>
          Json.obj(headers.zip(row).map { case (h, v) =>
            h -> Json.fromString(v)
          }: _*)
        ))
      )
      writeJSON(json, output) *> progressReporter.update(100)
      
    case ExportData.Tabular(headers, rows) =>
      val json = Json.obj(
        "headers" -> Json.fromValues(headers.map(Json.fromString)),
        "rows" -> Json.fromValues(rows.map(row =>
          Json.fromValues(row.map(Json.fromString))
        ))
      )
      writeJSON(json, output) *> progressReporter.update(100)
      
    case ExportData.SimulationJSON(json) =>
      writeJSON(json, output) *> progressReporter.update(100)
      
    case ExportData.GenericJSON(json) =>
      writeJSON(json, output) *> progressReporter.update(100)
  }
  
  private def exportToNetCDF(data: ExportData, output: Path): F[Unit] = {
    // NetCDF export would require additional dependencies
    // For now, we'll create a mock implementation
    for {
      _ <- progressReporter.update(50)
      _ <- Console[F].println("\n⚠️  NetCDF export is not yet implemented.")
      _ <- Console[F].println("   Creating a JSON file instead as a placeholder.")
      _ <- exportToJSON(data, output.resolveSibling(output.getFileName.toString.replace(".nc", ".json")))
      _ <- progressReporter.update(100)
    } yield ()
  }
  
  private def writeCSV(headers: List[String], rows: List[List[String]], path: Path): F[Unit] = 
    Sync[F].delay {
      val content = (headers :: rows).map(_.mkString(",")).mkString("\n")
      Files.write(path, content.getBytes("UTF-8"))
      ()
    }
  
  private def writeJSON(json: Json, path: Path): F[Unit] = 
    Sync[F].delay {
      Files.write(path, json.spaces2.getBytes("UTF-8"))
      ()
    }
  
  private def extractTimeSeriesFromJSON(json: Json): F[ExportData.TimeSeries] = Sync[F].delay {
    val cursor = json.hcursor
    
    val frames = cursor.downField("frames").as[List[Json]].getOrElse(List.empty)
    if (frames.isEmpty) throw new RuntimeException("No frames found in simulation data")
    
    val headers = List("timeStep", "activeFires", "burntArea", "treeDensity", "percolation")
    
    val rows = frames.map { frame =>
      val frameCursor = frame.hcursor
      val metrics = frameCursor.downField("metrics")
      
      List(
        frameCursor.downField("timeStep").as[Int].getOrElse(0).toString,
        metrics.downField("activeFires").as[Int].getOrElse(0).toString,
        metrics.downField("totalBurntArea").as[Int].getOrElse(0).toString,
        metrics.downField("treeDensity").as[Double].getOrElse(0.0).toString,
        metrics.downField("percolationIndicator").as[Double].getOrElse(0.0).toString
      )
    }
    
    ExportData.TimeSeries(headers, rows)
  }
  
  private def flattenJSON(json: Json): F[ExportData.Tabular] = Sync[F].delay {
    // Simple JSON flattening for tabular export
    val obj = json.asObject.getOrElse(JsonObject.empty)
    val headers = obj.keys.toList
    val values = headers.map(k => obj(k).map(_.noSpaces).getOrElse(""))
    
    ExportData.Tabular(headers, List(values))
  }
  
  private def getFileSize(path: Path): F[Long] = 
    Sync[F].delay(Files.size(path))
  
  private def formatFileSize(bytes: Long): String = {
    if (bytes < 1024) s"$bytes B"
    else if (bytes < 1024 * 1024) f"${bytes / 1024.0}%.1f KB"
    else if (bytes < 1024 * 1024 * 1024) f"${bytes / (1024.0 * 1024)}%.1f MB"
    else f"${bytes / (1024.0 * 1024 * 1024)}%.1f GB"
  }
}

// Data types for export
sealed trait ExportData

object ExportData {
  case class TimeSeries(headers: List[String], rows: List[List[String]]) extends ExportData
  case class GridSnapshot(headers: List[String], rows: List[List[String]]) extends ExportData
  case class Tabular(headers: List[String], rows: List[List[String]]) extends ExportData
  case class SimulationJSON(json: Json) extends ExportData
  case class GenericJSON(json: Json) extends ExportData
}