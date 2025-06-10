package io

import data._
import cats.effect.IO
import java.nio.file.Path

/**
 * Base trait for data exporters
 */
trait DataExporter {
  /**
   * Export a single snapshot
   */
  def exportSnapshot(snapshot: SimulationSnapshot, path: Path): IO[Unit]
  
  /**
   * Export a time series of snapshots
   */
  def exportTimeSeries(series: SimulationTimeSeries, path: Path): IO[Unit]
  
  /**
   * Export only metrics data (lighter weight)
   */
  def exportMetrics(metrics: TimeSeries[FireMetrics], path: Path): IO[Unit]
  
  /**
   * Get file extension for this exporter
   */
  def fileExtension: String
  
  /**
   * Get MIME type for this format
   */
  def mimeType: String
}

/**
 * Configuration for data export
 */
case class ExportConfig(
  compression: CompressionType = NoCompression,
  precision: DataPrecision = DoublePrecision,
  includeMetadata: Boolean = true,
  includeGridData: Boolean = true,
  includeSpatialMetrics: Boolean = true,
  includeEventHistory: Boolean = false,
  downsampleFactor: Option[Int] = None
)

/**
 * Compression options
 */
sealed trait CompressionType
case object NoCompression extends CompressionType
case object GzipCompression extends CompressionType
case object ZstdCompression extends CompressionType
case object LZ4Compression extends CompressionType

/**
 * Data precision options
 */
sealed trait DataPrecision
case object SinglePrecision extends DataPrecision
case object DoublePrecision extends DataPrecision

/**
 * Common export utilities
 */
object ExportUtils {
  /**
   * Create directory if it doesn't exist
   */
  def ensureDirectory(path: Path): IO[Unit] = IO {
    val parent = path.getParent
    if (parent != null && !parent.toFile.exists()) {
      parent.toFile.mkdirs()
    }
  }
  
  /**
   * Generate filename with timestamp
   */
  def generateFilename(baseName: String, extension: String): String = {
    val timestamp = java.time.Instant.now().toString.replace(":", "-").replace(".", "-")
    s"${baseName}_$timestamp.$extension"
  }
  
  /**
   * Convert grid to 2D array for export
   */
  def gridToArray[T](grid: models.Grid, extractor: models.Cell => T)(implicit ct: scala.reflect.ClassTag[T]): Array[Array[T]] = {
    Array.tabulate(grid.height, grid.width) { (y, x) =>
      extractor(grid(x, y))
    }
  }
  
  /**
   * Flatten grid data with coordinates
   */
  def flattenGridData[T](grid: models.Grid, extractor: models.Cell => T): List[(Int, Int, T)] = {
    (for {
      y <- 0 until grid.height
      x <- 0 until grid.width
    } yield (x, y, extractor(grid(x, y)))).toList
  }
  
  /**
   * Compress data if needed
   */
  def compressData(data: Array[Byte], compression: CompressionType): IO[Array[Byte]] = IO {
    import java.io._
    import java.util.zip.GZIPOutputStream
    
    compression match {
      case NoCompression => data
      case GzipCompression =>
        val baos = new ByteArrayOutputStream()
        val gzip = new GZIPOutputStream(baos)
        gzip.write(data)
        gzip.close()
        baos.toByteArray
      case _ => 
        // Other compression types would need external libraries
        data
    }
  }
}