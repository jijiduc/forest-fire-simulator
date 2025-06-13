package cli

import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import java.nio.file.{Path, Paths}

object CommandParser {
  
  val runCommand: Opts[RunCommand] = Opts.subcommand("run", "Run a forest fire simulation") {
    val region = Opts.option[String]("region", short = "r", help = s"Region to simulate (${Regions.all.mkString(", ")}, or finges, visp-stalden, leukerbad, sion-saviese, test)")
      .withDefault("finges")
      .validate(s"Invalid region. Must be one of: ${Regions.all.mkString(", ")} or a scenario ID")(r => 
        Regions.isValid(r) || io.geodata.RegionScenarios.byId(r).isDefined
      )
    
    val scenario = Opts.option[String]("scenario", short = "s", help = s"Climate scenario (${ClimateScenarios.all.mkString(", ")})")
      .withDefault(ClimateScenarios.Baseline)
      .validate(s"Invalid scenario. Must be one of: ${ClimateScenarios.all.mkString(", ")}")(ClimateScenarios.isValid)
    
    val year = Opts.option[Int]("year", short = "y", help = "Simulation year (2020, 2050, 2100)")
      .withDefault(2020)
      .validate("Year must be 2020, 2050, or 2100")(y => y == 2020 || y == 2050 || y == 2100)
    
    val steps = Opts.option[Int]("steps", short = "n", help = "Number of simulation steps")
      .withDefault(1000)
      .validate("Steps must be positive")(s => s > 0)
    
    val output = Opts.option[Path]("output", short = "o", help = "Output directory")
      .withDefault(Paths.get("output"))
    
    val scale = Opts.option[String]("scale", help = "Grid scale factor (full, half, quarter, tenth)")
      .withDefault("full")
      .validate("Invalid scale. Must be one of: full, half, quarter, tenth")(s => 
        io.geodata.ScaleFactor.fromString(s).isDefined
      )
    
    val useRealData = Opts.flag("real-data", help = "Use real geodata from Swiss federal services").orFalse
    
    (region, scenario, year, steps, output, scale, useRealData).mapN(RunCommand.apply)
  }
  
  val analyzeCommand: Opts[AnalyzeCommand] = Opts.subcommand("analyze", "Analyze simulation results") {
    val input = Opts.option[Path]("input", short = "i", help = "Input directory containing simulation results")
    
    val analysisType = Opts.option[String]("type", short = "t", help = s"Analysis type (${AnalysisTypes.all.mkString(", ")})")
      .withDefault(AnalysisTypes.PhaseTransition)
      .validate(s"Invalid analysis type. Must be one of: ${AnalysisTypes.all.mkString(", ")}")(AnalysisTypes.isValid)
    
    val output = Opts.option[Path]("output", short = "o", help = "Output directory")
      .withDefault(Paths.get("analysis"))
    
    (input, analysisType, output).mapN(AnalyzeCommand.apply)
  }
  
  val exportCommand: Opts[ExportCommand] = Opts.subcommand("export", "Export simulation data to different formats") {
    val input = Opts.option[Path]("input", short = "i", help = "Input file to export")
    
    val format = Opts.option[String]("format", short = "f", help = s"Export format (${ExportFormats.all.mkString(", ")})")
      .validate(s"Invalid format. Must be one of: ${ExportFormats.all.mkString(", ")}")(ExportFormats.isValid)
    
    val output = Opts.option[Path]("output", short = "o", help = "Output file path")
    
    (input, format, output).mapN(ExportCommand.apply)
  }
  
  val demoCommand: Opts[DemoCommand] = Opts.subcommand("demo", "Run demonstration scenarios") {
    val demoType = Opts.option[String]("type", short = "t", help = s"Demo type (${DemoTypes.all.mkString(", ")})")
      .withDefault(DemoTypes.QuickStart)
      .validate(s"Invalid demo type. Must be one of: ${DemoTypes.all.mkString(", ")}")(DemoTypes.isValid)
    
    demoType.map(DemoCommand.apply)
  }
  
  val configCommand: Opts[ConfigCommand] = Opts.subcommand("config", "Configuration management") {
    val action = Opts.argument[String]("action")
      .validate("Action must be 'show' or 'validate'")(a => a == "show" || a == "validate")
    
    action.map(ConfigCommand.apply)
  }
  
  val visualizeCommand: Opts[VisualizeCommand] = Opts.subcommand("visualize", "Launch visualization tools") {
    val input = Opts.option[Path]("input", short = "i", help = "Input simulation data file")
      .withDefault(Paths.get("output/simulation.json"))
    
    val vizType = Opts.option[String]("type", short = "t", help = s"Visualization type (${VisualizationTypes.all.mkString(", ")})")
      .withDefault(VisualizationTypes.WebViewer)
      .validate(s"Invalid visualization type. Must be one of: ${VisualizationTypes.all.mkString(", ")}")(VisualizationTypes.isValid)
    
    val port = Opts.option[Int]("port", short = "p", help = "Port for web server (default: 8080)")
      .withDefault(8080)
      .validate("Port must be between 1024 and 65535")(p => p >= 1024 && p <= 65535)
    
    (input, vizType, port).mapN(VisualizeCommand.apply)
  }
  
  val versionOpt: Opts[Unit] = Opts.flag("version", short = "v", help = "Show version information").void
  
  val commands: Opts[cli.Command] = 
    runCommand <+> analyzeCommand <+> exportCommand <+> demoCommand <+> configCommand <+> visualizeCommand
  
  val commandApp: Command[cli.Command] = Command(
    name = "forest-fire-simulator",
    header = "Forest Fire Simulator - Alpine Ecosystem Modeling",
    helpFlag = true
  )(commands)
  
  val versionCommand: Command[Unit] = Command(
    name = "forest-fire-simulator",
    header = "Forest Fire Simulator - Alpine Ecosystem Modeling"
  )(versionOpt)
  
  def parse(args: List[String]): Either[Help, cli.Command] = 
    commandApp.parse(args) match {
      case Left(help) =>
        // Check if it's a version request
        versionCommand.parse(args) match {
          case Right(_) => Left(Help.fromCommand(versionCommand))
          case Left(_) => Left(help)
        }
      case Right(cmd) => Right(cmd)
    }
}