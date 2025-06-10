package io.swiss

import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import org.http4s._
import org.http4s.client._
import org.http4s.client.dsl.io._
import org.http4s.circe._
import org.http4s.implicits._
import java.time.{LocalDate, LocalDateTime}
import java.nio.file.{Files, Path}
import models.Season

/**
 * STAC (SpatioTemporal Asset Catalog) API client for climate data
 */
class STACClient[F[_]: Async](client: Client[F]) {
  
  private val baseUrl = "https://data.geo.admin.ch/api/stac/v0.9"
  
  // STAC data models
  case class STACCatalog(
    id: String,
    description: String,
    links: List[STACLink]
  )
  
  case class STACCollection(
    id: String,
    title: String,
    description: String,
    extent: STACExtent,
    links: List[STACLink]
  )
  
  case class STACExtent(
    spatial: SpatialExtent,
    temporal: TemporalExtent
  )
  
  case class SpatialExtent(bbox: List[List[Double]])
  case class TemporalExtent(interval: List[List[Option[String]]])
  
  case class STACItem(
    id: String,
    geometry: Option[Json],
    properties: Map[String, Json],
    assets: Map[String, STACAsset],
    links: List[STACLink]
  )
  
  case class STACAsset(
    href: String,
    title: Option[String],
    `type`: Option[String],
    roles: Option[List[String]]
  )
  
  case class STACLink(
    href: String,
    rel: String,
    `type`: Option[String],
    title: Option[String]
  )
  
  case class ItemCollection(
    features: List[STACItem],
    links: List[STACLink]
  )
  
  implicit val stacLinkDecoder: Decoder[STACLink] = Decoder.instance { c =>
    for {
      href <- c.downField("href").as[String]
      rel <- c.downField("rel").as[String]
      tpe <- c.downField("type").as[Option[String]]
      title <- c.downField("title").as[Option[String]]
    } yield STACLink(href, rel, tpe, title)
  }
  
  implicit val stacAssetDecoder: Decoder[STACAsset] = Decoder.instance { c =>
    for {
      href <- c.downField("href").as[String]
      title <- c.downField("title").as[Option[String]]
      tpe <- c.downField("type").as[Option[String]]
      roles <- c.downField("roles").as[Option[List[String]]]
    } yield STACAsset(href, title, tpe, roles)
  }
  
  implicit val spatialExtentDecoder: Decoder[SpatialExtent] = Decoder.instance { c =>
    c.downField("bbox").as[List[List[Double]]].map(SpatialExtent)
  }
  
  implicit val temporalExtentDecoder: Decoder[TemporalExtent] = Decoder.instance { c =>
    c.downField("interval").as[List[List[Option[String]]]].map(TemporalExtent)
  }
  
  implicit def entityDecoder[F[_]: Concurrent]: EntityDecoder[F, ItemCollection] = jsonOf[F, ItemCollection]
  
  /**
   * Search for climate scenario items
   */
  def searchClimateScenarios(
    bbox: CoordinateSystems.LV95BoundingBox,
    scenario: SwissDataModels.ClimateScenario,
    timeRange: Option[(LocalDate, LocalDate)] = None
  ): F[List[STACItem]] = {
    
    // Convert LV95 bbox to WGS84 for STAC API
    val sw = bbox.copy(maxEast = bbox.minEast, maxNorth = bbox.minNorth).center.toWGS84
    val ne = bbox.copy(minEast = bbox.maxEast, minNorth = bbox.maxNorth).center.toWGS84
    
    val scenarioId = scenario match {
      case SwissDataModels.RCP26 => "RCP2.6"
      case SwissDataModels.RCP45 => "RCP4.5"
      case SwissDataModels.RCP85 => "RCP8.5"
    }
    
    val searchBody = Json.obj(
      "bbox" -> Json.arr(
        Json.fromDoubleOrNull(sw.longitude),
        Json.fromDoubleOrNull(sw.latitude),
        Json.fromDoubleOrNull(ne.longitude),
        Json.fromDoubleOrNull(ne.latitude)
      ),
      "collections" -> Json.arr(Json.fromString(s"ch.meteoswiss.climate-scenarios-$scenarioId")),
      "limit" -> Json.fromInt(100)
    )
    
    val uri = Uri.fromString(s"$baseUrl/search").toOption.get
    
    val request = Request[F](Method.POST, uri)
      .withEntity(searchBody)
    
    client.expect[ItemCollection](request).map(_.features)
  }
  
  /**
   * Download climate data asset
   */
  def downloadAsset(asset: STACAsset, outputPath: Path): F[Unit] = {
    val uri = Uri.fromString(asset.href).toOption.get
    
    client.stream(Request[F](uri = uri)).flatMap { response =>
      fs2.io.file.Files[F].writeAll(fs2.io.file.Path.fromNioPath(outputPath))(response.body)
    }.compile.drain
  }
  
  /**
   * Get climate projections for a specific location and time
   */
  def getClimateProjections(
    location: CoordinateSystems.LV95,
    scenario: SwissDataModels.ClimateScenario,
    year: Int
  ): F[ClimateProjection] = {
    
    // Create small bbox around the point
    val bbox = location.toBoundingBox(1000, 1000)
    
    for {
      items <- searchClimateScenarios(bbox, scenario)
      // Filter items by year
      relevantItems = items.filter { item =>
        item.properties.get("year").flatMap(_.asNumber).flatMap(_.toInt).contains(year)
      }
      projection <- if (relevantItems.nonEmpty) {
        extractProjectionFromItems(relevantItems, location)
      } else {
        Async[F].pure(ClimateProjection.default(scenario, year))
      }
    } yield projection
  }
  
  private def extractProjectionFromItems(
    items: List[STACItem],
    location: CoordinateSystems.LV95
  ): F[ClimateProjection] = Async[F].delay {
    // Extract climate variables from item properties
    val temperatureChanges = items.flatMap { item =>
      item.properties.get("temperature_change").flatMap(_.asNumber).map(_.toDouble)
    }
    val precipitationChanges = items.flatMap { item =>
      item.properties.get("precipitation_change").flatMap(_.asNumber).map(_.toDouble)
    }
    
    ClimateProjection(
      temperatureChange = if (temperatureChanges.nonEmpty) temperatureChanges.sum / temperatureChanges.length else 0.0,
      precipitationChange = if (precipitationChanges.nonEmpty) precipitationChanges.sum / precipitationChanges.length else 0.0,
      extremeEventMultiplier = 1.0 // Would be calculated from data
    )
  }
}

/**
 * Climate projection data
 */
case class ClimateProjection(
  temperatureChange: Double, // °C change from baseline
  precipitationChange: Double, // % change from baseline
  extremeEventMultiplier: Double // Multiplier for extreme weather frequency
)

object ClimateProjection {
  def default(scenario: SwissDataModels.ClimateScenario, year: Int): ClimateProjection = {
    // Default projections based on scenario and year
    val yearFactor = (year - 2020) / 80.0 // Linear interpolation to 2100
    
    scenario match {
      case SwissDataModels.RCP26 => ClimateProjection(
        temperatureChange = 1.0 * yearFactor,
        precipitationChange = -5.0 * yearFactor,
        extremeEventMultiplier = 1.0 + 0.2 * yearFactor
      )
      case SwissDataModels.RCP45 => ClimateProjection(
        temperatureChange = 2.0 * yearFactor,
        precipitationChange = -10.0 * yearFactor,
        extremeEventMultiplier = 1.0 + 0.5 * yearFactor
      )
      case SwissDataModels.RCP85 => ClimateProjection(
        temperatureChange = 4.0 * yearFactor,
        precipitationChange = -20.0 * yearFactor,
        extremeEventMultiplier = 1.0 + 1.0 * yearFactor
      )
    }
  }
}

/**
 * Climate data downloader
 */
class ClimateDataDownloader[F[_]: Async](stacClient: STACClient[F]) {
  
  /**
   * Download all climate scenarios for a region
   */
  def downloadClimateData(
    bbox: CoordinateSystems.LV95BoundingBox,
    outputDir: Path,
    scenarios: List[SwissDataModels.ClimateScenario] = List(
      SwissDataModels.RCP26,
      SwissDataModels.RCP45,
      SwissDataModels.RCP85
    ),
    years: List[Int] = List(2035, 2060, 2085)
  ): F[Unit] = {
    
    for {
      _ <- Async[F].delay(Files.createDirectories(outputDir))
      
      _ <- scenarios.traverse { scenario =>
        years.traverse { year =>
          downloadScenarioYear(bbox, scenario, year, outputDir)
        }
      }
      
      _ <- Async[F].delay(println(s"Climate data downloaded to $outputDir"))
    } yield ()
  }
  
  private def downloadScenarioYear(
    bbox: CoordinateSystems.LV95BoundingBox,
    scenario: SwissDataModels.ClimateScenario,
    year: Int,
    outputDir: Path
  ): F[Unit] = {
    
    val scenarioName = scenario match {
      case SwissDataModels.RCP26 => "rcp26"
      case SwissDataModels.RCP45 => "rcp45"
      case SwissDataModels.RCP85 => "rcp85"
    }
    
    for {
      _ <- Async[F].delay(println(s"Downloading $scenarioName for year $year..."))
      
      items <- stacClient.searchClimateScenarios(bbox, scenario)
      
      _ <- items.traverse { item =>
        item.assets.toList.traverse { case (name, asset) =>
          if (asset.`type`.contains("application/x-netcdf")) {
            val filename = s"${scenarioName}_${year}_$name.nc"
            stacClient.downloadAsset(asset, outputDir.resolve(filename))
          } else {
            Async[F].unit
          }
        }
      }
    } yield ()
  }
}