package io.geodata

import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import io.circe.parser._
import fs2.Stream
import java.nio.file.{Files, Path, Paths}
import io.swiss.CoordinateSystems.LV95
import models.VegetationType

// Define BoundingBox here to avoid circular dependencies
case class BoundingBox(
  min: LV95,
  max: LV95
)

trait GeoDataClient[F[_]] {
  def fetchElevation(bounds: BoundingBox, resolution: Int): F[Array[Array[Double]]]
  def fetchLandCover(bounds: BoundingBox): F[Array[Array[VegetationType]]]
  def fetchClimateData(bounds: BoundingBox, scenario: String, year: Int): F[Map[String, Double]]
}

class SwissGeoDataClient[F[_]: Async](
  client: Client[F],
  cacheDir: Path = Paths.get("cache/geodata")
) extends GeoDataClient[F] {
  
  // Base URLs for Swiss services
  private val wmsBaseUrl = "https://wms.geo.admin.ch"
  private val wmtsBaseUrl = "https://wmts.geo.admin.ch"
  
  // Ensure cache directory exists
  private def ensureCacheDir: F[Unit] = 
    Sync[F].delay(Files.createDirectories(cacheDir))
  
  // Generate cache key for requests
  private def cacheKey(dataType: String, bounds: BoundingBox, resolution: Int): String = {
    val minE = bounds.min.east.toInt
    val minN = bounds.min.north.toInt
    val maxE = bounds.max.east.toInt
    val maxN = bounds.max.north.toInt
    s"${dataType}_${minE}_${minN}_${maxE}_${maxN}_${resolution}m"
  }
  
  override def fetchElevation(bounds: BoundingBox, resolution: Int): F[Array[Array[Double]]] = {
    val layer = resolution match {
      case r if r <= 25 => "ch.swisstopo.digitales-hoehenmodell_25"
      case r if r <= 50 => "ch.swisstopo.digitales-hoehenmodell_50"
      case _ => "ch.swisstopo.digitales-hoehenmodell_100"
    }
    
    val width = ((bounds.max.east - bounds.min.east) / resolution).toInt
    val height = ((bounds.max.north - bounds.min.north) / resolution).toInt
    
    val cacheFile = cacheDir.resolve(s"${cacheKey("elevation", bounds, resolution)}.tif")
    
    for {
      _ <- ensureCacheDir
      exists <- Sync[F].delay(Files.exists(cacheFile))
      data <- if (exists) {
        loadFromCache(cacheFile, width, height)
      } else {
        fetchFromWMS(layer, bounds, width, height, cacheFile)
      }
    } yield data
  }
  
  override def fetchLandCover(bounds: BoundingBox): F[Array[Array[VegetationType]]] = {
    val layer = "ch.swisstopo.tlm-bb"
    val resolution = 25 // TLM is typically 25m resolution
    val width = ((bounds.max.east - bounds.min.east) / resolution).toInt
    val height = ((bounds.max.north - bounds.min.north) / resolution).toInt
    
    val cacheFile = cacheDir.resolve(s"${cacheKey("landcover", bounds, resolution)}.png")
    
    for {
      _ <- ensureCacheDir
      exists <- Sync[F].delay(Files.exists(cacheFile))
      imageData <- if (exists) {
        loadImageFromCache(cacheFile)
      } else {
        fetchImageFromWMS(layer, bounds, width, height, cacheFile)
      }
      vegetationTypes <- Sync[F].delay(parseLandCover(imageData, width, height))
    } yield vegetationTypes
  }
  
  override def fetchClimateData(bounds: BoundingBox, scenario: String, year: Int): F[Map[String, Double]] = {
    // For now, return interpolated climate data based on scenario
    // In a full implementation, this would query MeteoSwiss APIs
    Sync[F].pure {
      val baseTemp = 15.0 // Base temperature in Celsius
      val baseHumidity = 0.6
      val basePrecip = 800.0 // mm/year
      
      val (tempChange, humidityChange, precipChange) = scenario match {
        case "rcp26" => 
          val years = (year - 2020) / 80.0
          (1.0 * years, -0.05 * years, -0.05 * years)
        case "rcp45" =>
          val years = (year - 2020) / 80.0
          (2.0 * years, -0.1 * years, -0.1 * years)
        case "rcp85" =>
          val years = (year - 2020) / 80.0
          (4.0 * years, -0.2 * years, -0.2 * years)
        case _ => (0.0, 0.0, 0.0)
      }
      
      Map(
        "temperature" -> (baseTemp + tempChange),
        "humidity" -> Math.max(0.2, baseHumidity + humidityChange),
        "precipitation" -> Math.max(200, basePrecip + basePrecip * precipChange),
        "extreme_event_frequency" -> (1.0 + Math.abs(tempChange) * 0.2)
      )
    }
  }
  
  private def fetchFromWMS(
    layer: String,
    bounds: BoundingBox,
    width: Int,
    height: Int,
    cacheFile: Path
  ): F[Array[Array[Double]]] = {
    val bbox = s"${bounds.min.east},${bounds.min.north},${bounds.max.east},${bounds.max.north}"
    
    val uri = Uri.fromString(
      s"$wmsBaseUrl?" +
      s"SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap" +
      s"&LAYERS=$layer" +
      s"&BBOX=$bbox" +
      s"&WIDTH=$width&HEIGHT=$height" +
      s"&CRS=EPSG:2056" +
      s"&FORMAT=image/geotiff"
    ).toOption.get
    
    for {
      response <- client.expect[Array[Byte]](uri)
      _ <- Sync[F].delay(Files.write(cacheFile, response))
      data <- parseGeoTiff(response, width, height)
    } yield data
  }
  
  private def fetchImageFromWMS(
    layer: String,
    bounds: BoundingBox,
    width: Int,
    height: Int,
    cacheFile: Path
  ): F[Array[Byte]] = {
    val bbox = s"${bounds.min.east},${bounds.min.north},${bounds.max.east},${bounds.max.north}"
    
    val uri = Uri.fromString(
      s"$wmsBaseUrl?" +
      s"SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap" +
      s"&LAYERS=$layer" +
      s"&BBOX=$bbox" +
      s"&WIDTH=$width&HEIGHT=$height" +
      s"&CRS=EPSG:2056" +
      s"&FORMAT=image/png"
    ).toOption.get
    
    for {
      response <- client.expect[Array[Byte]](uri)
      _ <- Sync[F].delay(Files.write(cacheFile, response))
    } yield response
  }
  
  private def loadFromCache(cacheFile: Path, width: Int, height: Int): F[Array[Array[Double]]] = 
    for {
      bytes <- Sync[F].delay(Files.readAllBytes(cacheFile))
      data <- parseGeoTiff(bytes, width, height)
    } yield data
  
  private def loadImageFromCache(cacheFile: Path): F[Array[Byte]] = 
    Sync[F].delay(Files.readAllBytes(cacheFile))
  
  private def parseGeoTiff(bytes: Array[Byte], width: Int, height: Int): F[Array[Array[Double]]] = 
    Sync[F].delay {
      // Simplified mock parsing - in reality would need proper GeoTIFF parsing
      val result = Array.ofDim[Double](height, width)
      
      // Generate realistic elevation data for Swiss Alps
      val random = new scala.util.Random(42)
      val baseElevation = 1500.0 // meters
      
      for {
        y <- 0 until height
        x <- 0 until width
      } {
        // Simple elevation model with some noise
        val noise = random.nextGaussian() * 100
        val gradient = (y.toDouble / height) * 500 // North-south gradient
        result(y)(x) = baseElevation + gradient + noise
      }
      
      result
    }
  
  private def parseLandCover(imageBytes: Array[Byte], width: Int, height: Int): Array[Array[VegetationType]] = {
    val result = Array.ofDim[VegetationType](height, width)
    
    // Simplified vegetation distribution based on elevation zones
    val random = new scala.util.Random(42)
    
    for {
      y <- 0 until height
      x <- 0 until width
    } {
      // Mock elevation-based vegetation
      val elevation = 1500 + (y.toDouble / height) * 500
      
      result(y)(x) = elevation match {
        case e if e < 800 => VegetationType.DenseForest
        case e if e < 1200 => VegetationType.SparseForest
        case e if e < 1600 => VegetationType.Shrubland
        case e if e < 2000 => VegetationType.Grassland
        case _ => VegetationType.Barren
      }
      
      // Add some randomness
      if (random.nextDouble() < 0.05) {
        result(y)(x) = VegetationType.Water
      } else if (random.nextDouble() < 0.02) {
        result(y)(x) = VegetationType.Urban
      }
    }
    
    result
  }
}

object GeoDataClient {
  def apply[F[_]: Async]: Resource[F, GeoDataClient[F]] = {
    EmberClientBuilder
      .default[F]
      .build
      .map(client => new SwissGeoDataClient[F](client))
  }
}