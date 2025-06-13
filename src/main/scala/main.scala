import cats.effect._
import cats.effect.std.Console
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import cli._
import app._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object ForestFireSimulator extends CommandIOApp(
  name = "forest-fire-simulator",
  header = "Forest Fire Simulator - Alpine Ecosystem Modeling",
  version = "1.0.0"
) {
  
  implicit def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]
  
  override def main: Opts[IO[ExitCode]] = 
    CommandParser.commands.map { command =>
      for {
        // Load configuration
        config <- ConfigLoader.load[IO]()
        
        // Create progress reporter
        progressReporter <- ProgressReporter.console[IO]
        
        // Execute command
        exitCode <- command match {
          case cmd: RunCommand => 
            new SimulationRunner[IO](config, progressReporter).run(cmd)
            
          case cmd: AnalyzeCommand => 
            new AnalysisRunner[IO](config, progressReporter).run(cmd)
            
          case cmd: ExportCommand => 
            new ExportRunner[IO](config, progressReporter).run(cmd)
            
          case cmd: DemoCommand => 
            new DemoRunner[IO](config, progressReporter).run(cmd)
            
          case ConfigCommand("show") => 
            showConfiguration(config)
            
          case ConfigCommand("validate") => 
            validateConfiguration(config)
            
          case cmd: VisualizeCommand =>
            new VisualizationRunner[IO](config).run(cmd)
            
          case _ => 
            Console[IO].errorln("Unknown command").as(ExitCode.Error)
        }
        
      } yield exitCode
    }
  
  private def showConfiguration(config: AppConfig): IO[ExitCode] = for {
    _ <- Console[IO].println("\n⚙️  Current Configuration")
    _ <- Console[IO].println("=" * 60)
    _ <- Console[IO].println("\nSimulation Settings:")
    _ <- Console[IO].println(s"  Grid Size: ${config.simulation.defaultGridSize}")
    _ <- Console[IO].println(s"  Adaptive Time Step: ${config.simulation.adaptiveTimeStep}")
    _ <- Console[IO].println(s"  Boundary Condition: ${config.simulation.boundaryCondition}")
    
    _ <- Console[IO].println("\nFire Dynamics:")
    _ <- Console[IO].println(f"  Base Ignition Probability: ${config.simulation.fireDynamics.baseIgnitionProbability}%.6f")
    _ <- Console[IO].println(f"  Moisture Coefficient: ${config.simulation.fireDynamics.moistureCoefficient}%.2f")
    _ <- Console[IO].println(f"  Temperature Critical: ${config.simulation.fireDynamics.temperatureCritical}%.1f°C")
    _ <- Console[IO].println(f"  Wind Factor: ${config.simulation.fireDynamics.windFactor}%.4f")
    _ <- Console[IO].println(f"  Slope Factor: ${config.simulation.fireDynamics.slopeFactor}%.3f")
    
    _ <- Console[IO].println("\nRegions:")
    _ <- config.regions.toList.traverse { case (name, region) =>
      Console[IO].println(s"  $name: ${region.sizeKm}km × ${region.sizeKm}km at (${region.centerLat}, ${region.centerLon})")
    }
    
    _ <- Console[IO].println("\nExport Settings:")
    _ <- Console[IO].println(s"  Formats: ${config.exportConfig.formats.mkString(", ")}")
    _ <- Console[IO].println(s"  Compression: ${config.exportConfig.compression}")
    
    _ <- Console[IO].println("\nLogging:")
    _ <- Console[IO].println(s"  Level: ${config.logging.level}")
    _ <- Console[IO].println(s"  File: ${config.logging.file.getOrElse("Console only")}")
  } yield ExitCode.Success
  
  private def validateConfiguration(config: AppConfig): IO[ExitCode] = for {
    _ <- Console[IO].println("\n🔍 Validating Configuration")
    _ <- Console[IO].println("=" * 60)
    
    errors = validateConfig(config)
    
    _ <- if (errors.isEmpty) {
      Console[IO].println("\n✅ Configuration is valid!")
    } else {
      Console[IO].println(s"\n❌ Found ${errors.length} configuration errors:") *>
      errors.traverse_(error => Console[IO].println(s"   - $error"))
    }
    
  } yield if (errors.isEmpty) ExitCode.Success else ExitCode.Error
  
  private def validateConfig(config: AppConfig): List[String] = {
    val errors = List.newBuilder[String]
    
    // Validate simulation settings
    if (config.simulation.defaultGridSize < 10) 
      errors += "Grid size must be at least 10"
    if (config.simulation.defaultGridSize > 1000) 
      errors += "Grid size must not exceed 1000"
    
    // Validate fire dynamics
    val fd = config.simulation.fireDynamics
    if (fd.baseIgnitionProbability < 0 || fd.baseIgnitionProbability > 1)
      errors += "Base ignition probability must be between 0 and 1"
    if (fd.moistureCoefficient < 0)
      errors += "Moisture coefficient must be non-negative"
    if (fd.temperatureCritical < -50 || fd.temperatureCritical > 60)
      errors += "Critical temperature must be between -50°C and 60°C"
    if (fd.windFactor < 0)
      errors += "Wind factor must be non-negative"
    if (fd.slopeFactor < 0)
      errors += "Slope factor must be non-negative"
    
    // Validate regions
    config.regions.foreach { case (name, region) =>
      if (region.sizeKm < 1 || region.sizeKm > 100)
        errors += s"Region $name: size must be between 1 and 100 km"
      if (region.resolutionM < 10 || region.resolutionM > 1000)
        errors += s"Region $name: resolution must be between 10 and 1000 meters"
      if (region.centerLat < 45 || region.centerLat > 48)
        errors += s"Region $name: latitude must be between 45 and 48 (Switzerland)"
      if (region.centerLon < 5 || region.centerLon > 11)
        errors += s"Region $name: longitude must be between 5 and 11 (Switzerland)"
    }
    
    errors.result()
  }
}