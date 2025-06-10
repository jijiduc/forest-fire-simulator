package io.importers

import cats.effect._
import cats.implicits._
import io.swiss._
import models._
import models.{Terrain, Grid}
import simulation._
import org.http4s.client._
import java.nio.file.{Path, Paths, Files}
import scala.io.Source
import scala.util.Using

/**
 * Main data importer for Swiss geodata
 */
class SwissDataImporter[F[_]: Async](httpClient: Client[F]) {
  
  private val mapClient = new SwissMapClient[F](httpClient)
  private val stacClient = new STACClient[F](httpClient)
  
  /**
   * Import all data for a region and create simulation components
   */
  def importRegionData(
    region: CoordinateSystems.LV95BoundingBox,
    gridResolution: Int = 50, // meters per cell
    cacheDir: Option[Path] = None
  ): F[RegionData] = {
    
    val cache = cacheDir.getOrElse(Paths.get("cache"))
    
    for {
      _ <- Async[F].delay(Files.createDirectories(cache))
      
      // Download or load from cache
      elevationData <- loadOrDownloadElevation(region, cache)
      landCoverData <- loadOrDownloadLandCover(region, cache)
      
      // Convert to simulation grid
      terrain <- createTerrain(elevationData, landCoverData, gridResolution)
      vegetationMap <- createVegetationMap(landCoverData, gridResolution)
      
    } yield RegionData(
      bounds = region,
      terrain = terrain,
      vegetationMap = vegetationMap,
      elevationData = elevationData,
      landCoverData = landCoverData
    )
  }
  
  /**
   * Import climate data for different scenarios
   */
  def importClimateScenarios(
    location: CoordinateSystems.LV95,
    baseClimate: Climate,
    scenarios: List[SwissDataModels.ClimateScenario] = List(
      SwissDataModels.RCP26,
      SwissDataModels.RCP45,
      SwissDataModels.RCP85
    ),
    years: List[Int] = List(2035, 2060, 2085)
  ): F[Map[(SwissDataModels.ClimateScenario, Int), Climate]] = {
    
    scenarios.flatTraverse { scenario =>
      years.traverse { year =>
        for {
          projection <- stacClient.getClimateProjections(location, scenario, year)
          adjustedClimate = applyClimateProjection(baseClimate, projection)
        } yield (scenario, year) -> adjustedClimate
      }
    }.map(_.toMap)
  }
  
  /**
   * Create a Wallis-specific simulation setup
   */
  def createWallisSimulation(
    subregion: Option[CoordinateSystems.LV95BoundingBox] = None,
    gridResolution: Int = 100
  ): F[WallisSimulationSetup] = {
    
    val bbox = subregion.getOrElse(SwissDataModels.Cantons.Wallis.boundary)
    
    for {
      regionData <- importRegionData(bbox, gridResolution)
      
      // Create base climate (current conditions)
      baseClimate = Climate(
        season = Season.Summer,
        wind = Wind(225, 15.0), // SW wind typical for Wallis
        humidity = 0.45, // Relatively dry
        precipitation = 0.0
      )
      
      // Import climate scenarios
      climateScenarios <- importClimateScenarios(bbox.center, baseClimate)
      
      // Initialize grid
      grid = GridInitializer.initializeGrid(regionData.terrain, baseClimate)
      
      // Apply vegetation from land cover
      gridWithVegetation = applyVegetationMap(grid, regionData.vegetationMap)
      
    } yield WallisSimulationSetup(
      regionData = regionData,
      initialGrid = gridWithVegetation,
      baseClimate = baseClimate,
      climateScenarios = climateScenarios,
      parameters = WallisParameters()
    )
  }
  
  private def loadOrDownloadElevation(
    region: CoordinateSystems.LV95BoundingBox,
    cacheDir: Path
  ): F[SwissDataModels.ElevationData] = {
    val cachePath = cacheDir.resolve("elevation.json")
    
    if (Files.exists(cachePath)) {
      loadElevationFromCache(cachePath)
    } else {
      for {
        _ <- Async[F].delay(println("Downloading elevation data from swisstopo..."))
        data <- mapClient.wms.getElevationData(region)
        _ <- saveElevationToCache(data, cachePath)
      } yield data
    }
  }
  
  private def loadOrDownloadLandCover(
    region: CoordinateSystems.LV95BoundingBox,
    cacheDir: Path
  ): F[SwissDataModels.LandCoverData] = {
    val cachePath = cacheDir.resolve("landcover.json")
    
    if (Files.exists(cachePath)) {
      loadLandCoverFromCache(cachePath)
    } else {
      for {
        _ <- Async[F].delay(println("Downloading land cover data from swisstopo..."))
        data <- mapClient.wms.getLandCoverData(region)
        _ <- saveLandCoverToCache(data, cachePath)
      } yield data
    }
  }
  
  private def createTerrain(
    elevationData: SwissDataModels.ElevationData,
    landCoverData: SwissDataModels.LandCoverData,
    resolution: Int
  ): F[Terrain] = Async[F].delay {
    // Resample to simulation resolution
    val width = (elevationData.bounds.width / resolution).toInt
    val height = (elevationData.bounds.height / resolution).toInt
    
    val elevations = Array.ofDim[Double](height, width)
    
    for {
      y <- 0 until height
      x <- 0 until width
    } {
      val lv95 = CoordinateSystems.LV95(
        elevationData.bounds.minEast + x * resolution + resolution / 2,
        elevationData.bounds.minNorth + y * resolution + resolution / 2
      )
      
      elevations(y)(x) = elevationData.getElevation(lv95).getOrElse(0.0)
    }
    
    val vectorData = elevations.map(_.toVector).toVector
    Terrain(vectorData, width, height)
  }
  
  private def createVegetationMap(
    landCoverData: SwissDataModels.LandCoverData,
    resolution: Int
  ): F[Array[Array[VegetationType]]] = Async[F].delay {
    val width = (landCoverData.bounds.width / resolution).toInt
    val height = (landCoverData.bounds.height / resolution).toInt
    
    val vegetation = Array.ofDim[VegetationType](height, width)
    
    for {
      y <- 0 until height
      x <- 0 until width
    } {
      val lv95 = CoordinateSystems.LV95(
        landCoverData.bounds.minEast + x * resolution + resolution / 2,
        landCoverData.bounds.minNorth + y * resolution + resolution / 2
      )
      
      vegetation(y)(x) = landCoverData.getLandCover(lv95)
        .map(_.toVegetationType)
        .getOrElse(VegetationType.Barren)
    }
    
    vegetation
  }
  
  private def applyVegetationMap(grid: Grid, vegetationMap: Array[Array[VegetationType]]): Grid = {
    val cells = Vector.tabulate(grid.height, grid.width) { (y, x) =>
      val cell = grid(x, y)
      val vegetation = vegetationMap(y)(x)
      
      // Update cell based on vegetation type
      val newState = vegetation match {
        case VegetationType.DenseForest | VegetationType.SparseForest => Tree
        case VegetationType.Water | VegetationType.Urban | VegetationType.Barren => Empty
        case _ => if (scala.util.Random.nextDouble() < 0.3) Tree else Empty
      }
      
      cell.copy(
        state = newState,
        vegetationType = vegetation,
        moisture = vegetation match {
          case VegetationType.Water => 1.0
          case VegetationType.DenseForest => 0.6
          case VegetationType.Grassland => 0.4
          case VegetationType.Barren => 0.1
          case _ => 0.3
        }
      )
    }
    
    Grid(cells, grid.width, grid.height)
  }
  
  private def applyClimateProjection(base: Climate, projection: ClimateProjection): Climate = {
    base.copy(
      humidity = math.max(0.0, math.min(1.0, 
        base.humidity * (1.0 + projection.precipitationChange / 100.0)
      )),
      wind = base.wind.copy(
        speed = base.wind.speed * projection.extremeEventMultiplier
      )
    )
  }
  
  // Cache I/O methods
  private def loadElevationFromCache(path: Path): F[SwissDataModels.ElevationData] = 
    Async[F].delay {
      val json = new String(Files.readAllBytes(path))
      // Simplified - would use proper JSON parsing
      val dataPath = path.resolveSibling("elevation.dat")
      val bytes = Files.readAllBytes(dataPath)
      
      // Mock implementation - would parse properly
      SwissDataModels.ElevationData(
        SwissDataModels.Cantons.Wallis.boundary,
        25.0,
        Array.ofDim[Double](100, 100)
      )
    }
  
  private def loadLandCoverFromCache(path: Path): F[SwissDataModels.LandCoverData] =
    Async[F].delay {
      // Mock implementation
      SwissDataModels.LandCoverData(
        SwissDataModels.Cantons.Wallis.boundary,
        10.0,
        Array.ofDim[SwissDataModels.LandCoverType](100, 100)
      )
    }
  
  private def saveElevationToCache(data: SwissDataModels.ElevationData, path: Path): F[Unit] =
    Async[F].delay {
      // Implementation provided in MapServiceClients
      ()
    }
  
  private def saveLandCoverToCache(data: SwissDataModels.LandCoverData, path: Path): F[Unit] =
    Async[F].delay {
      // Implementation provided in MapServiceClients
      ()
    }
}

/**
 * Region data container
 */
case class RegionData(
  bounds: CoordinateSystems.LV95BoundingBox,
  terrain: Terrain,
  vegetationMap: Array[Array[VegetationType]],
  elevationData: SwissDataModels.ElevationData,
  landCoverData: SwissDataModels.LandCoverData
)

/**
 * Wallis simulation setup
 */
case class WallisSimulationSetup(
  regionData: RegionData,
  initialGrid: Grid,
  baseClimate: Climate,
  climateScenarios: Map[(SwissDataModels.ClimateScenario, Int), Climate],
  parameters: WallisParameters
)

/**
 * Wallis-specific parameters
 */
case class WallisParameters(
  // Fire danger thresholds
  humidityThresholds: Map[SwissDataModels.FireDangerLevel, Double] = Map(
    SwissDataModels.VeryHighDanger -> 0.2,
    SwissDataModels.HighDanger -> 0.3,
    SwissDataModels.ModerateDanger -> 0.4,
    SwissDataModels.LowDanger -> 0.5,
    SwissDataModels.VeryLowDanger -> 0.6
  ),
  
  // Elevation effects
  elevationWindMultiplier: Double = 0.0005, // Wind speed increase per meter
  elevationTemperatureGradient: Double = -0.0065, // °C per meter
  
  // Foehn wind effects (specific to Wallis)
  foehnWindDirection: Double = 135, // SE wind
  foehnSpeedMultiplier: Double = 2.0,
  foehnHumidityReduction: Double = 0.3
)