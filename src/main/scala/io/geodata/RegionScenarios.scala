package io.geodata

import io.swiss.CoordinateSystems.LV95

// Scale factors for testing different grid sizes
sealed trait ScaleFactor {
  def value: Double
  def name: String
  def description: String
}

object ScaleFactor {
  case object Full extends ScaleFactor {
    val value = 1.0
    val name = "full"
    val description = "Full resolution (25m cells)"
  }
  
  case object Half extends ScaleFactor {
    val value = 0.5
    val name = "half"
    val description = "Half resolution (50m cells)"
  }
  
  case object Quarter extends ScaleFactor {
    val value = 0.25
    val name = "quarter"
    val description = "Quarter resolution (100m cells)"
  }
  
  case object Tenth extends ScaleFactor {
    val value = 0.1
    val name = "tenth"
    val description = "One-tenth resolution (250m cells)"
  }
  
  def fromString(s: String): Option[ScaleFactor] = s.toLowerCase match {
    case "full" | "1.0" => Some(Full)
    case "half" | "0.5" => Some(Half)
    case "quarter" | "0.25" => Some(Quarter)
    case "tenth" | "0.1" => Some(Tenth)
    case _ => None
  }
  
  val all: List[ScaleFactor] = List(Full, Half, Quarter, Tenth)
}

// Region scenario with bounds and characteristics
case class RegionScenario(
  id: String,
  name: String,
  description: String,
  bounds: BoundingBox,
  baseResolution: Int, // meters per cell at full scale
  characteristics: RegionCharacteristics
) {
  def getResolution(scale: ScaleFactor): Int = 
    (baseResolution / scale.value).toInt
    
  def getGridSize(scale: ScaleFactor): (Int, Int) = {
    val resolution = getResolution(scale)
    val width = ((bounds.max.east - bounds.min.east) / resolution).toInt
    val height = ((bounds.max.north - bounds.min.north) / resolution).toInt
    (width, height)
  }
  
  def getCellCount(scale: ScaleFactor): Int = {
    val (w, h) = getGridSize(scale)
    w * h
  }
}

case class RegionCharacteristics(
  dominantVegetation: String,
  elevationRange: (Double, Double), // min, max in meters
  averageSlope: Double, // degrees
  fireReturnInterval: Int, // years
  foehnFrequency: Double, // fraction of days
  weatherStation: String, // MeteoSwiss station ID
  historicalFires: List[Int] // years with significant fires
)

object RegionScenarios {
  
  // Finges Forest - Primary study area
  val finges = RegionScenario(
    id = "finges",
    name = "Finges Forest (Pfynwald)",
    description = "Largest continuous pine forest in Switzerland, high fire risk",
    bounds = BoundingBox(
      min = LV95(2605000, 1125000),
      max = LV95(2615000, 1131000)
    ),
    baseResolution = 25,
    characteristics = RegionCharacteristics(
      dominantVegetation = "Scots Pine (Pinus sylvestris)",
      elevationRange = (530, 1200),
      averageSlope = 15.0,
      fireReturnInterval = 50,
      foehnFrequency = 0.15,
      weatherStation = "SIO", // Sion
      historicalFires = List(1996, 2003, 2011, 2018)
    )
  )
  
  // Visp-Stalden Triangle - Steep terrain
  val vispStalden = RegionScenario(
    id = "visp-stalden",
    name = "Visp-Stalden Triangle",
    description = "Steep slopes with frequent fires, good for slope effect studies",
    bounds = BoundingBox(
      min = LV95(2630000, 1120000),
      max = LV95(2645000, 1130000)
    ),
    baseResolution = 25,
    characteristics = RegionCharacteristics(
      dominantVegetation = "Mixed Pine-Larch forest",
      elevationRange = (650, 2000),
      averageSlope = 35.0,
      fireReturnInterval = 30,
      foehnFrequency = 0.20,
      weatherStation = "VIS", // Visp
      historicalFires = List(2007, 2015, 2021)
    )
  )
  
  // Leukerbad Valley - Isolated system
  val leukerbad = RegionScenario(
    id = "leukerbad",
    name = "Leukerbad Valley",
    description = "Isolated valley system, good for boundary condition studies",
    bounds = BoundingBox(
      min = LV95(2610000, 1135000),
      max = LV95(2618000, 1143000)
    ),
    baseResolution = 25,
    characteristics = RegionCharacteristics(
      dominantVegetation = "Spruce-Fir forest",
      elevationRange = (1000, 2200),
      averageSlope = 25.0,
      fireReturnInterval = 80,
      foehnFrequency = 0.08,
      weatherStation = "LEU", // Leukerbad
      historicalFires = List(1989, 2018)
    )
  )
  
  // Sion-Savièse - Urban interface
  val sionSaviese = RegionScenario(
    id = "sion-saviese",
    name = "Sion-Savièse Slopes",
    description = "Urban-wildland interface, important for risk assessment",
    bounds = BoundingBox(
      min = LV95(2590000, 1118000),
      max = LV95(2602000, 1126000)
    ),
    baseResolution = 25,
    characteristics = RegionCharacteristics(
      dominantVegetation = "Oak-Pine mixed forest",
      elevationRange = (500, 1500),
      averageSlope = 20.0,
      fireReturnInterval = 40,
      foehnFrequency = 0.12,
      weatherStation = "SIO", // Sion
      historicalFires = List(2003, 2011, 2020)
    )
  )
  
  // Small test area - for quick testing
  val testArea = RegionScenario(
    id = "test",
    name = "Test Area (Small)",
    description = "Small area for rapid testing and debugging",
    bounds = BoundingBox(
      min = LV95(2608000, 1127000),
      max = LV95(2610000, 1129000)
    ),
    baseResolution = 25,
    characteristics = RegionCharacteristics(
      dominantVegetation = "Mixed forest",
      elevationRange = (700, 900),
      averageSlope = 10.0,
      fireReturnInterval = 50,
      foehnFrequency = 0.10,
      weatherStation = "SIO",
      historicalFires = List()
    )
  )
  
  // All available scenarios
  val all: List[RegionScenario] = List(
    finges, vispStalden, leukerbad, sionSaviese, testArea
  )
  
  def byId(id: String): Option[RegionScenario] = 
    all.find(_.id == id)
  
  def printScenarioInfo(scenario: RegionScenario, scale: ScaleFactor): String = {
    val (width, height) = scenario.getGridSize(scale)
    val cells = scenario.getCellCount(scale)
    val resolution = scenario.getResolution(scale)
    val areaKm2 = (scenario.bounds.max.east - scenario.bounds.min.east) * 
                  (scenario.bounds.max.north - scenario.bounds.min.north) / 1_000_000
    
    s"""
    |${scenario.name}
    |${"-" * scenario.name.length}
    |Description: ${scenario.description}
    |Area: ${areaKm2.formatted("%.1f")} km²
    |Scale: ${scale.description}
    |Resolution: ${resolution}m per cell
    |Grid size: ${width} × ${height} = ${cells} cells
    |Elevation: ${scenario.characteristics.elevationRange._1.toInt}-${scenario.characteristics.elevationRange._2.toInt}m
    |Dominant vegetation: ${scenario.characteristics.dominantVegetation}
    |Average slope: ${scenario.characteristics.averageSlope}°
    |Weather station: ${scenario.characteristics.weatherStation}
    |Historical fires: ${scenario.characteristics.historicalFires.mkString(", ")}
    """.stripMargin
  }
}