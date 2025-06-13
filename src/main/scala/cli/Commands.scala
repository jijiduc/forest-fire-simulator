package cli

import java.nio.file.Path
import models.Climate

sealed trait Command

case class RunCommand(
  region: String,
  scenario: String,
  year: Int,
  steps: Int,
  output: Path,
  scale: String = "full",
  useRealData: Boolean = false
) extends Command

case class AnalyzeCommand(
  inputDir: Path,
  analysisType: String,
  output: Path
) extends Command

case class ExportCommand(
  inputFile: Path,
  format: String,
  output: Path
) extends Command

case class DemoCommand(
  demoType: String
) extends Command

case class ConfigCommand(
  action: String
) extends Command

case class VisualizeCommand(
  input: Path,
  visualizationType: String,
  port: Int = 8080
) extends Command

// Supporting types
object ClimateScenarios {
  val Baseline = "baseline"
  val RCP26 = "rcp26"
  val RCP45 = "rcp45"
  val RCP85 = "rcp85"
  
  val all = Set(Baseline, RCP26, RCP45, RCP85)
  
  def isValid(scenario: String): Boolean = all.contains(scenario.toLowerCase)
}

object Regions {
  val Wallis = "wallis"
  val Grisons = "grisons"
  val Ticino = "ticino"
  
  val all = Set(Wallis, Grisons, Ticino)
  
  def isValid(region: String): Boolean = all.contains(region.toLowerCase)
}

object ExportFormats {
  val CSV = "csv"
  val JSON = "json"
  val NetCDF = "netcdf"
  
  val all = Set(CSV, JSON, NetCDF)
  
  def isValid(format: String): Boolean = all.contains(format.toLowerCase)
}

object AnalysisTypes {
  val PhaseTransition = "phase-transition"
  val Statistics = "statistics"
  val Correlations = "correlations"
  val CriticalPoints = "critical-points"
  
  val all = Set(PhaseTransition, Statistics, Correlations, CriticalPoints)
  
  def isValid(analysisType: String): Boolean = all.contains(analysisType.toLowerCase)
}

object DemoTypes {
  val WallisClimate = "wallis-climate"
  val PhaseTransition = "phase-transition"
  val QuickStart = "quick-start"
  
  val all = Set(WallisClimate, PhaseTransition, QuickStart)
  
  def isValid(demoType: String): Boolean = all.contains(demoType.toLowerCase)
}

object VisualizationTypes {
  val WebViewer = "web"
  val ThreeD = "3d"
  val Notebook = "notebook"
  
  val all = Set(WebViewer, ThreeD, Notebook)
  
  def isValid(vizType: String): Boolean = all.contains(vizType.toLowerCase)
}