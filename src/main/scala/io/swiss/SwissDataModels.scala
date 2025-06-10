package io.swiss

import models._

/**
 * Swiss coordinate systems and transformations
 */
object CoordinateSystems {
  
  /**
   * Swiss LV95 (CH1903+) coordinate system
   */
  case class LV95(east: Double, north: Double) {
    def toWGS84: WGS84 = LV95.toWGS84(this)
    def toBoundingBox(widthMeters: Double, heightMeters: Double): LV95BoundingBox = 
      LV95BoundingBox(
        east - widthMeters / 2,
        north - heightMeters / 2,
        east + widthMeters / 2,
        north + heightMeters / 2
      )
  }
  
  object LV95 {
    /**
     * Convert LV95 to WGS84 coordinates
     * Based on simplified formulas from swisstopo
     */
    def toWGS84(lv95: LV95): WGS84 = {
      // Convert LV95 to the auxiliary values (y', x')
      val y_aux = (lv95.east - 2600000.0) / 1000000.0
      val x_aux = (lv95.north - 1200000.0) / 1000000.0
      
      // Calculate longitude and latitude in 10000" units
      val lng = 2.6779094 +
        4.728982 * y_aux +
        0.791484 * y_aux * x_aux +
        0.1306 * y_aux * x_aux * x_aux -
        0.0436 * y_aux * y_aux * y_aux
        
      val lat = 16.9023892 +
        3.238272 * x_aux -
        0.270978 * y_aux * y_aux -
        0.002528 * x_aux * x_aux -
        0.0447 * y_aux * y_aux * x_aux -
        0.0140 * x_aux * x_aux * x_aux
        
      // Convert to degrees
      val lng_deg = lng * 100.0 / 36.0
      val lat_deg = lat * 100.0 / 36.0
      
      WGS84(lat_deg, lng_deg)
    }
    
    /**
     * Convert WGS84 to LV95 coordinates
     */
    def fromWGS84(wgs84: WGS84): LV95 = {
      // Convert degrees to seconds and auxiliary values
      val lat_sec = wgs84.latitude * 3600.0
      val lng_sec = wgs84.longitude * 3600.0
      
      val lat_aux = (lat_sec - 169028.66) / 10000.0
      val lng_aux = (lng_sec - 26782.5) / 10000.0
      
      // Calculate Swiss coordinates
      val east = 2600072.37 +
        211455.93 * lng_aux -
        10938.51 * lng_aux * lat_aux -
        0.36 * lng_aux * lat_aux * lat_aux -
        44.54 * lng_aux * lng_aux * lng_aux
        
      val north = 1200147.07 +
        308807.95 * lat_aux +
        3745.25 * lng_aux * lng_aux +
        76.63 * lat_aux * lat_aux -
        194.56 * lng_aux * lng_aux * lat_aux +
        119.79 * lat_aux * lat_aux * lat_aux
        
      LV95(east, north)
    }
  }
  
  /**
   * WGS84 (GPS) coordinate system
   */
  case class WGS84(latitude: Double, longitude: Double) {
    def toLV95: LV95 = LV95.fromWGS84(this)
  }
  
  /**
   * Bounding box in LV95 coordinates
   */
  case class LV95BoundingBox(minEast: Double, minNorth: Double, maxEast: Double, maxNorth: Double) {
    def width: Double = maxEast - minEast
    def height: Double = maxNorth - minNorth
    def center: LV95 = LV95((minEast + maxEast) / 2, (minNorth + maxNorth) / 2)
    def contains(point: LV95): Boolean = 
      point.east >= minEast && point.east <= maxEast &&
      point.north >= minNorth && point.north <= maxNorth
      
    def toWMSBBox: String = s"$minEast,$minNorth,$maxEast,$maxNorth"
  }
}

/**
 * Swiss data structures
 */
object SwissDataModels {
  
  /**
   * Canton boundaries
   */
  case class Canton(
    name: String,
    abbreviation: String,
    boundary: CoordinateSystems.LV95BoundingBox
  )
  
  object Cantons {
    val Wallis = Canton(
      name = "Wallis",
      abbreviation = "VS",
      boundary = CoordinateSystems.LV95BoundingBox(
        minEast = 2550000,  // Approximate western boundary
        minNorth = 1075000, // Approximate southern boundary
        maxEast = 2660000,  // Approximate eastern boundary
        maxNorth = 1135000  // Approximate northern boundary
      )
    )
  }
  
  /**
   * Elevation model from DHM25
   */
  case class ElevationData(
    bounds: CoordinateSystems.LV95BoundingBox,
    resolution: Double, // meters per pixel
    data: Array[Array[Double]] // elevation in meters
  ) {
    def width: Int = data(0).length
    def height: Int = data.length
    
    def getElevation(lv95: CoordinateSystems.LV95): Option[Double] = {
      if (!bounds.contains(lv95)) return None
      
      val x = ((lv95.east - bounds.minEast) / resolution).toInt
      val y = ((lv95.north - bounds.minNorth) / resolution).toInt
      
      if (x >= 0 && x < width && y >= 0 && y < height) {
        Some(data(y)(x))
      } else None
    }
  }
  
  /**
   * Land cover data from TLM
   */
  case class LandCoverData(
    bounds: CoordinateSystems.LV95BoundingBox,
    resolution: Double,
    data: Array[Array[LandCoverType]]
  ) {
    def width: Int = data(0).length
    def height: Int = data.length
    
    def getLandCover(lv95: CoordinateSystems.LV95): Option[LandCoverType] = {
      if (!bounds.contains(lv95)) return None
      
      val x = ((lv95.east - bounds.minEast) / resolution).toInt
      val y = ((lv95.north - bounds.minNorth) / resolution).toInt
      
      if (x >= 0 && x < width && y >= 0 && y < height) {
        Some(data(y)(x))
      } else None
    }
  }
  
  /**
   * Land cover types based on Swiss TLM categories
   */
  sealed trait LandCoverType {
    def toVegetationType: VegetationType
  }
  
  case object ClosedForest extends LandCoverType {
    def toVegetationType: VegetationType = VegetationType.DenseForest
  }
  
  case object OpenForest extends LandCoverType {
    def toVegetationType: VegetationType = VegetationType.SparseForest
  }
  
  case object BushForest extends LandCoverType {
    def toVegetationType: VegetationType = VegetationType.Shrubland
  }
  
  case object Vineyard extends LandCoverType {
    def toVegetationType: VegetationType = VegetationType.Shrubland
  }
  
  case object GrasslandAgriculture extends LandCoverType {
    def toVegetationType: VegetationType = VegetationType.Grassland
  }
  
  case object WetlandReed extends LandCoverType {
    def toVegetationType: VegetationType = VegetationType.Grassland
  }
  
  case object StandingWater extends LandCoverType {
    def toVegetationType: VegetationType = VegetationType.Water
  }
  
  case object FlowingWater extends LandCoverType {
    def toVegetationType: VegetationType = VegetationType.Water
  }
  
  case object BuiltUp extends LandCoverType {
    def toVegetationType: VegetationType = VegetationType.Urban
  }
  
  case object BareRock extends LandCoverType {
    def toVegetationType: VegetationType = VegetationType.Barren
  }
  
  case object Glacier extends LandCoverType {
    def toVegetationType: VegetationType = VegetationType.Barren
  }
  
  /**
   * Climate scenario data
   */
  case class ClimateScenarioData(
    scenario: ClimateScenario,
    year: Int,
    season: Season,
    bounds: CoordinateSystems.LV95BoundingBox,
    resolution: Double,
    temperature: Array[Array[Double]], // Temperature anomaly in °C
    precipitation: Array[Array[Double]], // Precipitation change in %
    windSpeed: Option[Array[Array[Double]]] = None
  )
  
  sealed trait ClimateScenario
  case object RCP26 extends ClimateScenario // Low emissions
  case object RCP45 extends ClimateScenario // Medium emissions  
  case object RCP85 extends ClimateScenario // High emissions
  
  /**
   * Forest fire danger levels (Swiss system)
   */
  sealed trait FireDangerLevel {
    def ignitionProbabilityMultiplier: Double
    def spreadRateMultiplier: Double
  }
  
  case object VeryLowDanger extends FireDangerLevel {
    val ignitionProbabilityMultiplier = 0.2
    val spreadRateMultiplier = 0.5
  }
  
  case object LowDanger extends FireDangerLevel {
    val ignitionProbabilityMultiplier = 0.5
    val spreadRateMultiplier = 0.7
  }
  
  case object ModerateDanger extends FireDangerLevel {
    val ignitionProbabilityMultiplier = 1.0
    val spreadRateMultiplier = 1.0
  }
  
  case object HighDanger extends FireDangerLevel {
    val ignitionProbabilityMultiplier = 2.0
    val spreadRateMultiplier = 1.5
  }
  
  case object VeryHighDanger extends FireDangerLevel {
    val ignitionProbabilityMultiplier = 3.0
    val spreadRateMultiplier = 2.0
  }
}