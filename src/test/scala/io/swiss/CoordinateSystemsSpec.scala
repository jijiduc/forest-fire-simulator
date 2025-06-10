package io.swiss

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CoordinateSystemsSpec extends AnyFlatSpec with Matchers {
  
  "LV95 to WGS84 conversion" should "correctly transform Swiss coordinates" in {
    // Test with known coordinates
    // Bern: LV95 (2600000, 1200000) ≈ WGS84 (46.9511, 7.4386)
    val bern = CoordinateSystems.LV95(2600000, 1200000)
    val wgs84 = bern.toWGS84
    
    wgs84.latitude should be (46.9511 +- 0.01)
    wgs84.longitude should be (7.4386 +- 0.01)
  }
  
  "WGS84 to LV95 conversion" should "correctly transform GPS coordinates" in {
    // Zurich: WGS84 (47.3769, 8.5417) ≈ LV95 (2683111, 1248040)
    val zurich = CoordinateSystems.WGS84(47.3769, 8.5417)
    val lv95 = zurich.toLV95
    
    lv95.east should be (2683111.0 +- 200.0)
    lv95.north should be (1248040.0 +- 200.0)
  }
  
  "Round-trip conversion" should "preserve coordinates within tolerance" in {
    val original = CoordinateSystems.LV95(2635000, 1128000) // Near Visp
    val converted = original.toWGS84.toLV95
    
    converted.east should be (original.east +- 1.0)
    converted.north should be (original.north +- 1.0)
  }
  
  "LV95BoundingBox" should "correctly calculate dimensions" in {
    val bbox = CoordinateSystems.LV95BoundingBox(
      minEast = 2630000,
      minNorth = 1125000,
      maxEast = 2640000,
      maxNorth = 1135000
    )
    
    bbox.width should be (10000.0)
    bbox.height should be (10000.0)
    bbox.center should be (CoordinateSystems.LV95(2635000, 1130000))
  }
  
  it should "correctly check point containment" in {
    val bbox = CoordinateSystems.LV95BoundingBox(
      minEast = 2630000,
      minNorth = 1125000,
      maxEast = 2640000,
      maxNorth = 1135000
    )
    
    bbox.contains(CoordinateSystems.LV95(2635000, 1130000)) should be (true)
    bbox.contains(CoordinateSystems.LV95(2620000, 1130000)) should be (false)
    bbox.contains(CoordinateSystems.LV95(2635000, 1140000)) should be (false)
  }
  
  it should "generate correct WMS bbox string" in {
    val bbox = CoordinateSystems.LV95BoundingBox(
      minEast = 2630000,
      minNorth = 1125000,
      maxEast = 2640000,
      maxNorth = 1135000
    )
    
    bbox.toWMSBBox should be ("2630000.0,1125000.0,2640000.0,1135000.0")
  }
}

class SwissDataModelsSpec extends AnyFlatSpec with Matchers {
  
  "Canton Wallis" should "have correct boundaries" in {
    val wallis = SwissDataModels.Cantons.Wallis
    
    wallis.name should be ("Wallis")
    wallis.abbreviation should be ("VS")
    wallis.boundary.minEast should be (2550000.0)
    wallis.boundary.maxEast should be (2660000.0)
    wallis.boundary.minNorth should be (1075000.0)
    wallis.boundary.maxNorth should be (1135000.0)
  }
  
  "ElevationData" should "retrieve elevation for valid coordinates" in {
    val elevationData = SwissDataModels.ElevationData(
      bounds = CoordinateSystems.LV95BoundingBox(
        minEast = 2630000,
        minNorth = 1125000,
        maxEast = 2640000,
        maxNorth = 1135000
      ),
      resolution = 100.0,
      data = Array.tabulate(100, 100) { (y, x) =>
        600.0 + x * 10.0 + y * 5.0 // Simple elevation model
      }
    )
    
    val elevation = elevationData.getElevation(
      CoordinateSystems.LV95(2635000, 1130000)
    )
    
    elevation should be (defined)
    elevation.get should be > 600.0
  }
  
  it should "return None for coordinates outside bounds" in {
    val elevationData = SwissDataModels.ElevationData(
      bounds = CoordinateSystems.LV95BoundingBox(
        minEast = 2630000,
        minNorth = 1125000,
        maxEast = 2640000,
        maxNorth = 1135000
      ),
      resolution = 100.0,
      data = Array.ofDim[Double](100, 100)
    )
    
    val elevation = elevationData.getElevation(
      CoordinateSystems.LV95(2620000, 1130000) // Outside bounds
    )
    
    elevation should be (None)
  }
  
  "LandCoverType" should "convert to appropriate vegetation types" in {
    SwissDataModels.ClosedForest.toVegetationType should be (models.VegetationType.DenseForest)
    SwissDataModels.OpenForest.toVegetationType should be (models.VegetationType.SparseForest)
    SwissDataModels.GrasslandAgriculture.toVegetationType should be (models.VegetationType.Grassland)
    SwissDataModels.StandingWater.toVegetationType should be (models.VegetationType.Water)
    SwissDataModels.BuiltUp.toVegetationType should be (models.VegetationType.Urban)
    SwissDataModels.BareRock.toVegetationType should be (models.VegetationType.Barren)
  }
  
  "FireDangerLevel" should "have appropriate multipliers" in {
    SwissDataModels.VeryLowDanger.ignitionProbabilityMultiplier should be < 0.5
    SwissDataModels.VeryHighDanger.ignitionProbabilityMultiplier should be > 2.0
    
    SwissDataModels.VeryLowDanger.spreadRateMultiplier should be < 1.0
    SwissDataModels.VeryHighDanger.spreadRateMultiplier should be > 1.5
  }
  
  "ClimateScenario" should "be properly defined" in {
    val scenarios = List(
      SwissDataModels.RCP26,
      SwissDataModels.RCP45,
      SwissDataModels.RCP85
    )
    
    scenarios.size should be (3)
  }
}