package io.swiss

import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import org.http4s._
import org.http4s.client._
import org.http4s.client.dsl.io._
import org.http4s.implicits._
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.ByteArrayInputStream
import java.nio.file.{Files, Path, Paths}
import scala.util.Try

/**
 * WMS (Web Map Service) client for Swiss federal geodata
 */
class WMSClient[F[_]: Async](client: Client[F]) {
  
  private val baseUrl = "https://wms.geo.admin.ch"
  
  /**
   * Get map image from WMS service
   */
  def getMapImage(
    layers: String,
    bbox: CoordinateSystems.LV95BoundingBox,
    width: Int,
    height: Int,
    format: String = "image/png",
    epsg: String = "2056" // EPSG:2056 is LV95
  ): F[Array[Byte]] = {
    
    val params = Map(
      "SERVICE" -> "WMS",
      "VERSION" -> "1.3.0",
      "REQUEST" -> "GetMap",
      "LAYERS" -> layers,
      "BBOX" -> bbox.toWMSBBox,
      "WIDTH" -> width.toString,
      "HEIGHT" -> height.toString,
      "FORMAT" -> format,
      "CRS" -> s"EPSG:$epsg",
      "TRANSPARENT" -> "TRUE"
    )
    
    val uri = Uri.fromString(baseUrl).toOption.get
      .withQueryParams(params)
    
    client.expect[Array[Byte]](uri)
  }
  
  /**
   * Get elevation data from DHM25 layer
   */
  def getElevationData(
    bbox: CoordinateSystems.LV95BoundingBox,
    resolution: Int = 25 // meters per pixel
  ): F[SwissDataModels.ElevationData] = {
    
    val width = (bbox.width / resolution).toInt
    val height = (bbox.height / resolution).toInt
    
    for {
      imageBytes <- getMapImage(
        layers = "ch.swisstopo.digitales-hoehenmodell_25",
        bbox = bbox,
        width = width,
        height = height,
        format = "image/tiff"
      )
      elevationData <- parseElevationTiff(imageBytes, bbox, resolution)
    } yield elevationData
  }
  
  /**
   * Get land cover data from TLM layer
   */
  def getLandCoverData(
    bbox: CoordinateSystems.LV95BoundingBox,
    resolution: Int = 10
  ): F[SwissDataModels.LandCoverData] = {
    
    val width = (bbox.width / resolution).toInt
    val height = (bbox.height / resolution).toInt
    
    for {
      imageBytes <- getMapImage(
        layers = "ch.swisstopo.tlm-bb",
        bbox = bbox,
        width = width,
        height = height
      )
      landCoverData <- parseLandCoverImage(imageBytes, bbox, resolution)
    } yield landCoverData
  }
  
  private def parseElevationTiff(
    bytes: Array[Byte],
    bbox: CoordinateSystems.LV95BoundingBox,
    resolution: Int
  ): F[SwissDataModels.ElevationData] = Async[F].delay {
    // Simplified parsing - in production would use proper GeoTIFF library
    val image = ImageIO.read(new ByteArrayInputStream(bytes))
    val width = image.getWidth
    val height = image.getHeight
    
    val data = Array.ofDim[Double](height, width)
    
    for {
      y <- 0 until height
      x <- 0 until width
    } {
      val pixel = image.getRGB(x, y)
      // Extract elevation from pixel value (simplified)
      val elevation = ((pixel & 0xFFFF).toDouble / 10.0) + 400.0 // Adjust base elevation
      data(height - 1 - y)(x) = elevation // Flip Y axis
    }
    
    SwissDataModels.ElevationData(bbox, resolution, data)
  }
  
  private def parseLandCoverImage(
    bytes: Array[Byte],
    bbox: CoordinateSystems.LV95BoundingBox,
    resolution: Int
  ): F[SwissDataModels.LandCoverData] = Async[F].delay {
    import SwissDataModels._
    
    val image = ImageIO.read(new ByteArrayInputStream(bytes))
    val width = image.getWidth
    val height = image.getHeight
    
    val data = Array.ofDim[LandCoverType](height, width)
    
    for {
      y <- 0 until height
      x <- 0 until width
    } {
      val pixel = image.getRGB(x, y)
      // Map colors to land cover types (simplified mapping)
      val landCover = (pixel & 0xFF) match {
        case g if g < 50 => StandingWater
        case g if g < 80 => ClosedForest
        case g if g < 110 => OpenForest
        case g if g < 140 => BushForest
        case g if g < 170 => GrasslandAgriculture
        case g if g < 200 => BuiltUp
        case g if g < 230 => BareRock
        case _ => Glacier
      }
      data(height - 1 - y)(x) = landCover
    }
    
    SwissDataModels.LandCoverData(bbox, resolution, data)
  }
}

/**
 * WMTS (Web Map Tile Service) client for Swiss federal geodata
 */
class WMTSClient[F[_]: Async](client: Client[F]) {
  
  private val baseUrl = "https://wmts.geo.admin.ch"
  
  case class TileCoord(zoom: Int, col: Int, row: Int)
  
  /**
   * Get a single tile from WMTS service
   */
  def getTile(
    layer: String,
    tileCoord: TileCoord,
    format: String = "png",
    time: Option[String] = None
  ): F[Array[Byte]] = {
    
    val timePart = time.map(t => s"/$t").getOrElse("")
    val url = s"$baseUrl/1.0.0/$layer/default$timePart/2056/${tileCoord.zoom}/${tileCoord.col}/${tileCoord.row}.$format"
    
    val uri = Uri.fromString(url).toOption.get
    client.expect[Array[Byte]](uri)
  }
  
  /**
   * Calculate tile coordinates for a given bounding box and zoom level
   */
  def calculateTiles(bbox: CoordinateSystems.LV95BoundingBox, zoomLevel: Int): List[TileCoord] = {
    // Swiss WMTS tile matrix definitions
    val tileSize = 256
    val matrixSizes = Map(
      0 -> (1, 1),
      1 -> (2, 2),
      2 -> (4, 4),
      // ... up to zoom level 28
    )
    
    // Simplified calculation - in production would use proper tile matrix
    val resolution = 4000.0 / math.pow(2, zoomLevel) // meters per pixel
    val tileWidth = tileSize * resolution
    
    val minCol = ((bbox.minEast - 2420000) / tileWidth).toInt
    val maxCol = ((bbox.maxEast - 2420000) / tileWidth).toInt
    val minRow = ((1350000 - bbox.maxNorth) / tileWidth).toInt  
    val maxRow = ((1350000 - bbox.minNorth) / tileWidth).toInt
    
    for {
      col <- minCol to maxCol
      row <- minRow to maxRow
    } yield TileCoord(zoomLevel, col, row)
  }.toList
  
  /**
   * Download and stitch tiles for a region
   */
  def getTilesForRegion(
    layer: String,
    bbox: CoordinateSystems.LV95BoundingBox,
    zoomLevel: Int = 10
  ): F[BufferedImage] = {
    
    val tiles = calculateTiles(bbox, zoomLevel)
    
    for {
      tileImages <- tiles.traverse { coord =>
        getTile(layer, coord).map { bytes =>
          (coord, ImageIO.read(new ByteArrayInputStream(bytes)))
        }
      }
      stitched <- stitchTiles(tileImages, bbox, zoomLevel)
    } yield stitched
  }
  
  private def stitchTiles(
    tiles: List[(TileCoord, BufferedImage)],
    bbox: CoordinateSystems.LV95BoundingBox,
    zoomLevel: Int
  ): F[BufferedImage] = Async[F].delay {
    
    if (tiles.isEmpty) {
      new BufferedImage(256, 256, BufferedImage.TYPE_INT_ARGB)
    } else {
      val minCol = tiles.map(_._1.col).min
      val maxCol = tiles.map(_._1.col).max
      val minRow = tiles.map(_._1.row).min
      val maxRow = tiles.map(_._1.row).max
      
      val width = (maxCol - minCol + 1) * 256
      val height = (maxRow - minRow + 1) * 256
      
      val result = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
      val g = result.createGraphics()
      
      tiles.foreach { case (coord, image) =>
        val x = (coord.col - minCol) * 256
        val y = (coord.row - minRow) * 256
        g.drawImage(image, x, y, null)
      }
      
      g.dispose()
      result
    }
  }
}

/**
 * Combined map service client
 */
class SwissMapClient[F[_]: Async](httpClient: Client[F]) {
  
  val wms = new WMSClient[F](httpClient)
  val wmts = new WMTSClient[F](httpClient)
  
  /**
   * Download all necessary data for a region
   */
  def downloadRegionData(
    bbox: CoordinateSystems.LV95BoundingBox,
    outputDir: Path
  ): F[Unit] = {
    
    for {
      // Ensure output directory exists
      _ <- Async[F].delay(Files.createDirectories(outputDir))
      
      // Download elevation data
      _ <- Async[F].delay(println("Downloading elevation data..."))
      elevationData <- wms.getElevationData(bbox, resolution = 25)
      _ <- saveElevationData(elevationData, outputDir.resolve("elevation.json"))
      
      // Download land cover data
      _ <- Async[F].delay(println("Downloading land cover data..."))
      landCoverData <- wms.getLandCoverData(bbox, resolution = 10)
      _ <- saveLandCoverData(landCoverData, outputDir.resolve("landcover.json"))
      
      // Download visual reference map
      _ <- Async[F].delay(println("Downloading reference map..."))
      mapImage <- wmts.getTilesForRegion("ch.swisstopo.pixelkarte-farbe", bbox, zoomLevel = 12)
      _ <- Async[F].delay {
        ImageIO.write(mapImage, "png", outputDir.resolve("reference_map.png").toFile)
      }
      
      _ <- Async[F].delay(println(s"Data downloaded to $outputDir"))
    } yield ()
  }
  
  private def saveElevationData(data: SwissDataModels.ElevationData, path: Path): F[Unit] = 
    Async[F].delay {
      // Save as simple JSON for now
      val json = s"""{
        "bounds": {
          "minEast": ${data.bounds.minEast},
          "minNorth": ${data.bounds.minNorth},
          "maxEast": ${data.bounds.maxEast},
          "maxNorth": ${data.bounds.maxNorth}
        },
        "resolution": ${data.resolution},
        "width": ${data.width},
        "height": ${data.height}
      }"""
      Files.write(path, json.getBytes)
      
      // Save elevation data as binary file
      val dataPath = path.resolveSibling("elevation.dat")
      val bytes = data.data.flatten.flatMap { d =>
        java.nio.ByteBuffer.allocate(8).putDouble(d).array()
      }
      Files.write(dataPath, bytes)
    }
  
  private def saveLandCoverData(data: SwissDataModels.LandCoverData, path: Path): F[Unit] =
    Async[F].delay {
      val json = s"""{
        "bounds": {
          "minEast": ${data.bounds.minEast},
          "minNorth": ${data.bounds.minNorth},
          "maxEast": ${data.bounds.maxEast},
          "maxNorth": ${data.bounds.maxNorth}
        },
        "resolution": ${data.resolution},
        "width": ${data.width},
        "height": ${data.height}
      }"""
      Files.write(path, json.getBytes)
      
      // Save land cover types as simple byte array
      val dataPath = path.resolveSibling("landcover.dat")
      val bytes = data.data.flatten.map { lc =>
        lc match {
          case SwissDataModels.ClosedForest => 1.toByte
          case SwissDataModels.OpenForest => 2.toByte
          case SwissDataModels.BushForest => 3.toByte
          case SwissDataModels.Vineyard => 4.toByte
          case SwissDataModels.GrasslandAgriculture => 5.toByte
          case SwissDataModels.WetlandReed => 6.toByte
          case SwissDataModels.StandingWater => 7.toByte
          case SwissDataModels.FlowingWater => 8.toByte
          case SwissDataModels.BuiltUp => 9.toByte
          case SwissDataModels.BareRock => 10.toByte
          case SwissDataModels.Glacier => 11.toByte
        }
      }
      Files.write(dataPath, bytes)
    }
}