package io.importers

import cats.effect._
import cats.implicits._
import io.swiss._
import io.geodata._
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
  private val geoDataClient = new SwissGeoDataClient[F](httpClient)
  
  /**
   * Import all data for a region and create simulation components
   */
  def importRegionData(
    region: CoordinateSystems.LV95BoundingBox,
    gridResolution: Int = 50, // meters per cell
    cacheDir: Option[Path] = None,
    useRealData: Boolean = false
  ): F[RegionData] = {
    
    val cache = cacheDir.getOrElse(Paths.get("cache"))
    
    if (useRealData) {
      // Use real geodata through GeoDataClient
      for {
        _ <- Async[F].delay(Files.createDirectories(cache))
        
        // Create proper bounding box
        bounds = io.geodata.BoundingBox(
          min = CoordinateSystems.LV95(region.minEast, region.minNorth),
          max = CoordinateSystems.LV95(region.maxEast, region.maxNorth)
        )
        
        // Fetch real data
        elevationArray <- geoDataClient.fetchElevation(bounds, gridResolution)
        vegetationArray <- geoDataClient.fetchLandCover(bounds)
        
        // Convert to our data structures
        width = ((region.maxEast - region.minEast) / gridResolution).toInt
        height = ((region.maxNorth - region.minNorth) / gridResolution).toInt
        
        terrain = Terrain(
          elevationMap = elevationArray.map(_.toVector).toVector,
          width = width,
          height = height
        )
        
        elevationData = SwissDataModels.ElevationData(
          bounds = region,
          resolution = gridResolution.toDouble,
          data = elevationArray
        )
        
        landCoverData = SwissDataModels.LandCoverData(
          bounds = region,
          resolution = gridResolution.toDouble,
          data = vegetationArray.map(_.map(veg => 
            LandCoverTypeExtensions.fromVegetationType(veg)
          ))
        )
        
      } yield RegionData(
        bounds = region,
        terrain = terrain,
        vegetationMap = vegetationArray,
        elevationData = elevationData,
        landCoverData = landCoverData
      )
    } else {
      // Original implementation with mock data
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
   * Create a region-specific simulation setup using scenarios
   */
  def createRegionSimulation(
    scenario: RegionScenario,
    scale: ScaleFactor,
    useRealData: Boolean = false,
    climateScenario: String = "baseline",
    year: Int = 2020
  ): F[RegionSimulationSetup] = {
    
    val resolution = scenario.getResolution(scale)
    val (width, height) = scenario.getGridSize(scale)
    
    // Convert bounds to LV95BoundingBox
    val lv95Bounds = CoordinateSystems.LV95BoundingBox(
      minEast = scenario.bounds.min.east,
      maxEast = scenario.bounds.max.east,
      minNorth = scenario.bounds.min.north,
      maxNorth = scenario.bounds.max.north
    )
    
    for {
      regionData <- importRegionData(lv95Bounds, resolution, useRealData = useRealData)
      
      // Fetch climate data if using real data
      climateData <- if (useRealData) {
        geoDataClient.fetchClimateData(scenario.bounds, climateScenario, year)
      } else {
        Async[F].pure(Map(
          "temperature" -> 15.0,
          "humidity" -> 0.5,
          "precipitation" -> 800.0,
          "extreme_event_frequency" -> 1.0
        ))
      }
      
      // Create climate based on region and scenario
      baseClimate = createClimateFromData(scenario, climateData)
      
      // Initialize grid
      grid = GridInitializer.initializeGrid(regionData.terrain, baseClimate)
      
      // Apply vegetation from land cover
      gridWithVegetation = applyVegetationMap(grid, regionData.vegetationMap)
      
    } yield RegionSimulationSetup(
      scenario = scenario,
      scale = scale,
      regionData = regionData,
      initialGrid = gridWithVegetation,
      baseClimate = baseClimate,
      parameters = createRegionParameters(scenario)
    )
  }
  
  /**
   * Create a Wallis-specific simulation setup (legacy compatibility)
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

  private def createClimateFromData(scenario: RegionScenario, data: Map[String, Double]): Climate = {
    val temp = data.getOrElse("temperature", 15.0)
    val humidity = data.getOrElse("humidity", 0.5)
    
    // Apply Foehn effects if applicable
    val (windDir, windSpeed, adjustedHumidity) = if (scenario.characteristics.foehnFrequency > 0.1) {
      (135.0, 15.0 * 1.5, humidity * 0.7) // SE Foehn wind
    } else {
      (225.0, 10.0, humidity) // Default SW wind
    }
    
    Climate(
      season = Season.Summer,
      wind = Wind(windDir, windSpeed),
      humidity = adjustedHumidity,
      precipitation = 0.0
    )
  }
  
  private def createRegionParameters(scenario: RegionScenario): RegionParameters = {
    RegionParameters(
      elevationRange = scenario.characteristics.elevationRange,
      averageSlope = scenario.characteristics.averageSlope,
      dominantVegetation = scenario.characteristics.dominantVegetation,
      fireReturnInterval = scenario.characteristics.fireReturnInterval,
      foehnFrequency = scenario.characteristics.foehnFrequency,
      elevationWindMultiplier = 0.0005,
      elevationTemperatureGradient = -0.0065
    )
  }

// Extension to SwissDataModels.LandCoverType
object LandCoverTypeExtensions {
  def fromVegetationType(veg: VegetationType): SwissDataModels.LandCoverType = veg match {
      case VegetationType.DenseForest => SwissDataModels.ClosedForest
      case VegetationType.SparseForest => SwissDataModels.OpenForest
      case VegetationType.Shrubland => SwissDataModels.BushForest
      case VegetationType.Grassland => SwissDataModels.GrasslandAgriculture
      case VegetationType.Water => SwissDataModels.StandingWater
      case VegetationType.Urban => SwissDataModels.BuiltUp
      case VegetationType.Barren => SwissDataModels.BareRock
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
 * Generic region simulation setup
 */
case class RegionSimulationSetup(
  scenario: RegionScenario,
  scale: ScaleFactor,
  regionData: RegionData,
  initialGrid: Grid,
  baseClimate: Climate,
  parameters: RegionParameters
)

/**
 * Region-specific parameters
 */
case class RegionParameters(
  elevationRange: (Double, Double),
  averageSlope: Double,
  dominantVegetation: String,
  fireReturnInterval: Int,
  foehnFrequency: Double,
  elevationWindMultiplier: Double,
  elevationTemperatureGradient: Double
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