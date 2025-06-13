package cli

import cats.effect._
import cats.implicits._
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import models._
import simulation._

case class AppConfig(
  simulation: SimulationSettings,
  regions: Map[String, RegionConfig],
  exportConfig: ExportConfig,
  logging: LoggingConfig
)

case class SimulationSettings(
  defaultGridSize: Int,
  adaptiveTimeStep: Boolean,
  boundaryCondition: String,
  fireDynamics: FireDynamicsConfig
)

case class FireDynamicsConfig(
  baseIgnitionProbability: Double,
  moistureCoefficient: Double,
  temperatureCritical: Double,
  temperatureScale: Double,
  windFactor: Double,
  slopeFactor: Double
)

case class RegionConfig(
  centerLat: Double,
  centerLon: Double,
  sizeKm: Int,
  resolutionM: Int
)

case class ExportConfig(
  formats: List[String],
  compression: Boolean
)

case class LoggingConfig(
  level: String,
  file: Option[String]
)

object ConfigLoader {
  
  def load[F[_]: Sync](configPath: Option[Path] = None): F[AppConfig] = Sync[F].delay {
    val config = configPath match {
      case Some(path) if Files.exists(path) =>
        ConfigFactory.parseFile(path.toFile).withFallback(ConfigFactory.load())
      case _ =>
        // Try to load from default locations
        val defaultPath = Paths.get("forest-fire-simulator.conf")
        if (Files.exists(defaultPath)) {
          ConfigFactory.parseFile(defaultPath.toFile).withFallback(ConfigFactory.load())
        } else {
          ConfigFactory.load()
        }
    }
    
    parseConfig(config)
  }
  
  private def parseConfig(config: Config): AppConfig = {
    val simulation = parseSimulation(config.getConfig("simulation"))
    val regions = parseRegions(config.getConfig("regions"))
    val exportConf = parseExport(config.getConfig("export"))
    val logging = parseLogging(config.getConfig("logging"))
    
    AppConfig(simulation, regions, exportConf, logging)
  }
  
  private def parseSimulation(config: Config): SimulationSettings = {
    val fireDynamics = config.getConfig("fire-dynamics")
    
    SimulationSettings(
      defaultGridSize = config.getInt("default-grid-size"),
      adaptiveTimeStep = config.getBoolean("adaptive-time-step"),
      boundaryCondition = config.getString("boundary-condition"),
      fireDynamics = FireDynamicsConfig(
        baseIgnitionProbability = fireDynamics.getDouble("base-ignition-probability"),
        moistureCoefficient = fireDynamics.getDouble("moisture-coefficient"),
        temperatureCritical = fireDynamics.getDouble("temperature-critical"),
        temperatureScale = fireDynamics.getDouble("temperature-scale"),
        windFactor = fireDynamics.getDouble("wind-factor"),
        slopeFactor = fireDynamics.getDouble("slope-factor")
      )
    )
  }
  
  private def parseRegions(config: Config): Map[String, RegionConfig] = {
    config.root().keySet().asScala.map { key =>
      val regionConfig = config.getConfig(key)
      key -> RegionConfig(
        centerLat = regionConfig.getDouble("center-lat"),
        centerLon = regionConfig.getDouble("center-lon"),
        sizeKm = regionConfig.getInt("size-km"),
        resolutionM = regionConfig.getInt("resolution-m")
      )
    }.toMap
  }
  
  private def parseExport(config: Config): ExportConfig = {
    ExportConfig(
      formats = config.getStringList("formats").asScala.toList,
      compression = config.getBoolean("compression")
    )
  }
  
  private def parseLogging(config: Config): LoggingConfig = {
    LoggingConfig(
      level = config.getString("level"),
      file = if (config.hasPath("file")) Some(config.getString("file")) else None
    )
  }
  
  def createDefault: AppConfig = AppConfig(
    simulation = SimulationSettings(
      defaultGridSize = 100,
      adaptiveTimeStep = true,
      boundaryCondition = "absorbing",
      fireDynamics = FireDynamicsConfig(
        baseIgnitionProbability = 0.0001,
        moistureCoefficient = 2.5,
        temperatureCritical = 30.0,
        temperatureScale = 5.0,
        windFactor = 0.1783,
        slopeFactor = 3.533
      )
    ),
    regions = Map(
      "wallis" -> RegionConfig(46.2, 7.5, 10, 100),
      "grisons" -> RegionConfig(46.7, 9.5, 15, 100),
      "ticino" -> RegionConfig(46.2, 8.8, 12, 100)
    ),
    exportConfig = ExportConfig(
      formats = List("csv", "json"),
      compression = true
    ),
    logging = LoggingConfig(
      level = "INFO",
      file = Some("simulation.log")
    )
  )
}