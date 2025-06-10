package io.importers

import io.swiss._
import models._
import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.http4s._
import org.http4s.client._
import java.nio.file.{Files, Paths}

class SwissDataImporterSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  
  // Mock HTTP client that returns empty responses
  val mockHttpClient: Client[IO] = Client.fromHttpApp[IO](HttpApp[IO] { req =>
    IO.pure(Response[IO](Status.Ok).withEntity(Array.empty[Byte]))
  })
  
  "SwissDataImporter" should "create terrain from elevation data" in {
    val importer = new SwissDataImporter[IO](mockHttpClient)
    
    val elevationData = SwissDataModels.ElevationData(
      bounds = CoordinateSystems.LV95BoundingBox(
        minEast = 2630000,
        minNorth = 1125000,
        maxEast = 2631000,  // 1km
        maxNorth = 1126000  // 1km
      ),
      resolution = 25.0,
      data = Array.tabulate(40, 40) { (y, x) =>
        600.0 + x * 10.0 + y * 5.0
      }
    )
    
    val landCoverData = SwissDataModels.LandCoverData(
      bounds = elevationData.bounds,
      resolution = 25.0,
      data = Array.fill(40, 40)(SwissDataModels.ClosedForest)
    )
    
    // Use reflection to test private method
    val createTerrainMethod = importer.getClass.getDeclaredMethod(
      "createTerrain",
      classOf[SwissDataModels.ElevationData],
      classOf[SwissDataModels.LandCoverData],
      classOf[Int]
    )
    createTerrainMethod.setAccessible(true)
    
    val terrainIO = createTerrainMethod.invoke(
      importer,
      elevationData,
      landCoverData,
      Integer.valueOf(50)
    ).asInstanceOf[IO[Terrain]]
    
    terrainIO.asserting { terrain =>
      terrain.width should be (20)  // 1000m / 50m
      terrain.height should be (20)
      terrain.elevationAt(0, 0) should be > 500.0
    }
  }
  
  "Climate projection application" should "modify climate correctly" in {
    val importer = new SwissDataImporter[IO](mockHttpClient)
    
    val baseClimate = Climate(
      season = Season.Summer,
      wind = Wind(180, 10.0),
      humidity = 0.5,
      precipitation = 0.0
    )
    
    val projection = ClimateProjection(
      temperatureChange = 2.0,
      precipitationChange = -20.0,  // 20% drier
      extremeEventMultiplier = 1.5
    )
    
    // Use reflection to test private method
    val applyProjectionMethod = importer.getClass.getDeclaredMethod(
      "applyClimateProjection",
      classOf[Climate],
      classOf[ClimateProjection]
    )
    applyProjectionMethod.setAccessible(true)
    
    val adjustedClimate = applyProjectionMethod.invoke(
      importer,
      baseClimate,
      projection
    ).asInstanceOf[Climate]
    
    adjustedClimate.humidity should be (0.4 +- 0.01)  // 20% reduction
    adjustedClimate.wind.speed should be (15.0 +- 0.01)  // 1.5x multiplier
  }
  
  "RegionData" should "contain all necessary components" in {
    val bounds = CoordinateSystems.LV95BoundingBox(
      minEast = 2630000,
      minNorth = 1125000,
      maxEast = 2631000,
      maxNorth = 1126000
    )
    
    val terrain = Terrain(
      Vector.tabulate(20, 20)((y, x) => 600.0 + x * 10.0),
      20, 20
    )
    
    val vegetationMap: Array[Array[VegetationType]] = Array.fill(20, 20)(VegetationType.DenseForest)
    
    val elevationData = SwissDataModels.ElevationData(
      bounds, 50.0, Array.ofDim[Double](20, 20)
    )
    
    val landCoverData = SwissDataModels.LandCoverData(
      bounds, 50.0, Array.ofDim[SwissDataModels.LandCoverType](20, 20)
    )
    
    val regionData = RegionData(
      bounds = bounds,
      terrain = terrain,
      vegetationMap = vegetationMap,
      elevationData = elevationData,
      landCoverData = landCoverData
    )
    
    regionData.bounds should be (bounds)
    regionData.terrain should be (terrain)
    regionData.vegetationMap.length should be (20)
  }
  
  "WallisParameters" should "have reasonable defaults" in {
    val params = WallisParameters()
    
    // Check humidity thresholds are ordered correctly
    params.humidityThresholds(SwissDataModels.VeryHighDanger) should be < 
      params.humidityThresholds(SwissDataModels.VeryLowDanger)
    
    // Check elevation effects
    params.elevationWindMultiplier should be > 0.0
    params.elevationTemperatureGradient should be < 0.0  // Temperature decreases with elevation
    
    // Check Foehn wind parameters
    params.foehnWindDirection should be (135.0)  // SE wind
    params.foehnSpeedMultiplier should be > 1.0
    params.foehnHumidityReduction should be > 0.0
  }
}