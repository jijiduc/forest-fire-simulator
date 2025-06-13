package cli

import simulation._
import analysis._
import models._
import app._

trait OutputFormatter {
  def formatSummary(state: SimulationState): String
  def formatMetrics(metrics: SimulationMetrics): String
  def formatPhaseAnalysis(analysis: BasicPhaseAnalysisResult): String
  def formatTable[A](headers: List[String], rows: List[List[A]]): String
  def formatError(error: Throwable): String
}

object ConsoleFormatter extends OutputFormatter {
  
  private val separator = "─" * 80
  
  def formatSummary(state: SimulationState): String = {
    s"""
    |$separator
    |SIMULATION SUMMARY
    |$separator
    |Time Step: ${state.timeStep}
    |Elapsed Time: ${f"${state.elapsedTime}%.2f"} seconds
    |Grid Size: ${state.grid.width} × ${state.grid.height}
    |
    |FINAL METRICS:
    |${formatMetrics(state.metrics)}
    |$separator
    """.stripMargin
  }
  
  def formatMetrics(metrics: SimulationMetrics): String = {
    s"""
    |  Active Fires: ${metrics.activeFires}
    |  Total Burnt Area: ${metrics.totalBurntArea} cells
    |  Tree Density: ${f"${metrics.treeDensity * 100}%.1f"}%
    |  Average Moisture: ${f"${metrics.averageMoisture * 100}%.1f"}%
    |  Average Temperature: ${f"${metrics.averageTemperature}%.1f"}°C
    |  Fire Spread Rate: ${f"${metrics.fireSpreadRate}%.2f"} cells/step
    |  Percolation Indicator: ${f"${metrics.percolationIndicator}%.3f"}
    """.stripMargin.trim
  }
  
  def formatPhaseAnalysis(analysis: BasicPhaseAnalysisResult): String = {
    val phaseStr = analysis.phase match {
      case SubCritical => "SUB-CRITICAL (isolated fires)"
      case Critical => "CRITICAL (power-law distribution)"
      case SuperCritical => "SUPER-CRITICAL (system-spanning fires)"
    }
    
    val criticalPoints = analysis.criticalPoints.map { cp =>
      s"    ${cp.parameter}: ${f"${cp.value}%.4f"} ± ${f"${cp.uncertainty}%.4f"}"
    }.mkString("\n")
    
    s"""
    |$separator
    |PHASE TRANSITION ANALYSIS
    |$separator
    |Phase: $phaseStr
    |
    |Order Parameters:
    |  Burnt Fraction: ${f"${analysis.orderParameters.burntFraction}%.3f"}
    |  Largest Cluster: ${f"${analysis.orderParameters.largestClusterRatio}%.3f"}
    |  Percolation: ${if (analysis.orderParameters.percolationIndicator > 0.5) "YES" else "NO"}
    |  Cluster Density: ${f"${analysis.orderParameters.clusterDensity}%.3f"}
    |  Avg Cluster Size: ${f"${analysis.orderParameters.averageClusterSize}%.1f"}
    |
    |Critical Points:
    |$criticalPoints
    |
    |Universality Class: ${analysis.universalityClass.getOrElse("Unknown")}
    |$separator
    """.stripMargin
  }
  
  def formatTable[A](headers: List[String], rows: List[List[A]]): String = {
    if (headers.isEmpty || rows.isEmpty) return ""
    
    val allRows = headers :: rows.map(_.map(_.toString))
    val columnWidths = allRows.transpose.map(col => col.map(_.length).max + 2)
    
    def formatRow(row: List[String]): String = {
      row.zip(columnWidths).map { case (cell, width) =>
        s" ${cell.padTo(width - 1, ' ')}"
      }.mkString("│", "│", "│")
    }
    
    val headerRow = formatRow(headers)
    val separatorRow = columnWidths.map(w => "─" * w).mkString("├", "┼", "┤")
    val dataRows = rows.map(row => formatRow(row.map(_.toString)))
    
    val topBorder = columnWidths.map(w => "─" * w).mkString("┌", "┬", "┐")
    val bottomBorder = columnWidths.map(w => "─" * w).mkString("└", "┴", "┘")
    
    s"""
    |$topBorder
    |$headerRow
    |$separatorRow
    |${dataRows.mkString("\n")}
    |$bottomBorder
    """.stripMargin
  }
  
  def formatError(error: Throwable): String = {
    val errorType = error.getClass.getSimpleName
    val message = Option(error.getMessage).getOrElse("No error message")
    val cause = Option(error.getCause).map(c => s"\nCaused by: ${c.getMessage}").getOrElse("")
    
    s"""
    |$separator
    |ERROR: $errorType
    |$separator
    |$message$cause
    |
    |For more details, check the log file.
    |$separator
    """.stripMargin
  }
}

object JsonFormatter extends OutputFormatter {
  import io.circe._
  import io.circe.syntax._
  import io.circe.generic.auto._
  
  def formatSummary(state: SimulationState): String = {
    Json.obj(
      "timeStep" -> state.timeStep.asJson,
      "elapsedTime" -> state.elapsedTime.asJson,
      "gridSize" -> Json.obj(
        "width" -> state.grid.width.asJson,
        "height" -> state.grid.height.asJson
      ),
      "metrics" -> formatMetricsJson(state.metrics)
    ).spaces2
  }
  
  def formatMetrics(metrics: SimulationMetrics): String = 
    formatMetricsJson(metrics).spaces2
  
  private def formatMetricsJson(metrics: SimulationMetrics): Json = {
    Json.obj(
      "activeFires" -> metrics.activeFires.asJson,
      "totalBurntArea" -> metrics.totalBurntArea.asJson,
      "treeDensity" -> metrics.treeDensity.asJson,
      "averageMoisture" -> metrics.averageMoisture.asJson,
      "averageTemperature" -> metrics.averageTemperature.asJson,
      "fireSpreadRate" -> metrics.fireSpreadRate.asJson,
      "percolationIndicator" -> metrics.percolationIndicator.asJson
    )
  }
  
  def formatPhaseAnalysis(analysis: BasicPhaseAnalysisResult): String = {
    Json.obj(
      "phase" -> analysis.phase.toString.asJson,
      "orderParameters" -> Json.obj(
        "burntFraction" -> analysis.orderParameters.burntFraction.asJson,
        "largestClusterRatio" -> analysis.orderParameters.largestClusterRatio.asJson,
        "percolationIndicator" -> analysis.orderParameters.percolationIndicator.asJson,
        "clusterDensity" -> analysis.orderParameters.clusterDensity.asJson,
        "averageClusterSize" -> analysis.orderParameters.averageClusterSize.asJson
      ),
      "criticalPoints" -> analysis.criticalPoints.map { cp =>
        Json.obj(
          "parameter" -> cp.parameter.asJson,
          "value" -> cp.value.asJson,
          "uncertainty" -> cp.uncertainty.asJson
        )
      }.asJson,
      "universalityClass" -> analysis.universalityClass.asJson
    ).spaces2
  }
  
  def formatTable[A](headers: List[String], rows: List[List[A]]): String = {
    val data = rows.map(row => headers.zip(row.map(_.toString)).toMap)
    data.asJson.spaces2
  }
  
  def formatError(error: Throwable): String = {
    Json.obj(
      "error" -> error.getClass.getSimpleName.asJson,
      "message" -> Option(error.getMessage).asJson,
      "cause" -> Option(error.getCause).map(_.getMessage).asJson
    ).spaces2
  }
}