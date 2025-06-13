package app

import cats.effect._
import cats.effect.std.Console
import cats.implicits._
import cli._
import analysis._
import io.exporters._
import simulation._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import java.nio.file.{Files, Path}
import scala.io.Source

class AnalysisRunner[F[_]: Async: Console](
  config: AppConfig,
  progressReporter: ProgressReporter[F]
) {
  
  implicit def logger: Logger[F] = Slf4jLogger.getLogger[F]
  
  def run(command: AnalyzeCommand): F[ExitCode] = {
    for {
      _ <- logger.info(s"Starting ${command.analysisType} analysis")
      _ <- Console[F].println(s"\n📊 Analysis: ${command.analysisType}")
      _ <- Console[F].println("=" * 60)
      
      // Check input directory exists
      _ <- checkInputDirectory(command.inputDir)
      
      // Create output directory
      _ <- createOutputDirectory(command.output)
      
      // Load simulation data
      _ <- Console[F].println("\n📂 Loading simulation data...")
      simulationData <- loadSimulationData(command.inputDir)
      
      // Perform analysis based on type
      _ <- Console[F].println(s"\n🔍 Performing ${command.analysisType} analysis...")
      result <- command.analysisType match {
        case AnalysisTypes.PhaseTransition => performPhaseTransitionAnalysis(simulationData, command.output)
        case AnalysisTypes.Statistics => performStatisticalAnalysis(simulationData, command.output)
        case AnalysisTypes.Correlations => performCorrelationAnalysis(simulationData, command.output)
        case AnalysisTypes.CriticalPoints => performCriticalPointAnalysis(simulationData, command.output)
      }
      
      _ <- Console[F].println(s"\n✅ Analysis complete! Results saved to ${command.output}/")
      
    } yield ExitCode.Success
  }.handleErrorWith { error =>
    for {
      _ <- logger.error(error)("Analysis failed")
      _ <- Console[F].errorln(ConsoleFormatter.formatError(error))
    } yield ExitCode.Error
  }
  
  private def checkInputDirectory(path: Path): F[Unit] = 
    Sync[F].delay {
      if (!Files.exists(path) || !Files.isDirectory(path)) {
        throw new IllegalArgumentException(s"Input directory does not exist: $path")
      }
    }
  
  private def createOutputDirectory(path: Path): F[Unit] = 
    Sync[F].delay(Files.createDirectories(path))
  
  private def loadSimulationData(inputDir: Path): F[SimulationData] = Sync[F].delay {
    val resultsFile = inputDir.resolve("simulation_results.csv")
    if (!Files.exists(resultsFile)) {
      throw new IllegalArgumentException(s"Simulation results not found: $resultsFile")
    }
    
    // Parse CSV data (simplified - in real implementation would use proper CSV parser)
    val source = Source.fromFile(resultsFile.toFile)
    try {
      val lines = source.getLines().toList
      val headers = lines.head.split(",").map(_.trim)
      val data = lines.tail.map(_.split(",").map(_.trim))
      
      // Extract time series data
      val timeSteps = data.map(row => row(headers.indexOf("timeStep")).toInt)
      val burntFractions = data.map(row => row(headers.indexOf("burntFraction")).toDouble)
      val activeFires = data.map(row => row(headers.indexOf("activeFires")).toInt)
      val percolation = data.map(row => row(headers.indexOf("percolationIndicator")).toDouble)
      
      SimulationData(
        timeSteps = timeSteps,
        burntFractions = burntFractions,
        activeFires = activeFires,
        percolationIndicators = percolation,
        metadata = Map(
          "gridSize" -> data.head(headers.indexOf("gridSize")),
          "scenario" -> data.head(headers.indexOf("scenario"))
        )
      )
    } finally {
      source.close()
    }
  }
  
  private def performPhaseTransitionAnalysis(
    data: SimulationData,
    outputDir: Path
  ): F[Unit] = for {
    _ <- progressReporter.start(100, "Analyzing phase transitions")
    
    // Identify phase based on final state
    phase = identifyPhase(data)
    _ <- progressReporter.update(25)
    
    // Calculate order parameters
    orderParams = calculateOrderParameters(data)
    _ <- progressReporter.update(50)
    
    // Find critical points
    criticalPoints = findCriticalPoints(data)
    _ <- progressReporter.update(75)
    
    // Generate report
    report = generatePhaseTransitionReport(phase, orderParams, criticalPoints)
    _ <- writeReport(outputDir.resolve("phase_transition_analysis.txt"), report)
    _ <- progressReporter.finish("Phase transition analysis complete")
    
    _ <- Console[F].println(s"\nPhase identified: $phase")
    _ <- Console[F].println(s"Critical behavior: ${if (criticalPoints.nonEmpty) "Yes" else "No"}")
  } yield ()
  
  private def performStatisticalAnalysis(
    data: SimulationData,
    outputDir: Path
  ): F[Unit] = for {
    _ <- progressReporter.start(100, "Computing statistics")
    
    // Basic statistics
    stats = computeBasicStatistics(data)
    _ <- progressReporter.update(33)
    
    // Time series analysis
    trends = analyzeTrends(data)
    _ <- progressReporter.update(66)
    
    // Generate statistical report
    report = generateStatisticalReport(stats, trends)
    _ <- writeReport(outputDir.resolve("statistical_analysis.txt"), report)
    _ <- progressReporter.finish("Statistical analysis complete")
    
    _ <- Console[F].println(f"\nMean burnt fraction: ${stats.meanBurntFraction}%.3f")
    _ <- Console[F].println(f"Fire frequency: ${stats.fireFrequency}%.2f fires/step")
  } yield ()
  
  private def performCorrelationAnalysis(
    data: SimulationData,
    outputDir: Path
  ): F[Unit] = for {
    _ <- progressReporter.start(100, "Analyzing correlations")
    
    // Temporal correlations
    temporalCorr = computeTemporalCorrelations(data)
    _ <- progressReporter.update(50)
    
    // Generate correlation report
    report = generateCorrelationReport(temporalCorr)
    _ <- writeReport(outputDir.resolve("correlation_analysis.txt"), report)
    _ <- progressReporter.finish("Correlation analysis complete")
    
    _ <- Console[F].println(f"\nAutocorrelation time: ${temporalCorr.autocorrelationTime}%.1f steps")
  } yield ()
  
  private def performCriticalPointAnalysis(
    data: SimulationData,
    outputDir: Path
  ): F[Unit] = for {
    _ <- progressReporter.start(100, "Finding critical points")
    
    // Analyze fluctuations
    fluctuations = analyzeFluctuations(data)
    _ <- progressReporter.update(50)
    
    // Find peaks in susceptibility
    criticalRegions = findSusceptibilityPeaks(fluctuations)
    _ <- progressReporter.update(100)
    
    // Generate report
    report = generateCriticalPointReport(criticalRegions)
    _ <- writeReport(outputDir.resolve("critical_point_analysis.txt"), report)
    _ <- progressReporter.finish("Critical point analysis complete")
    
    _ <- Console[F].println(s"\nCritical regions found: ${criticalRegions.size}")
  } yield ()
  
  // Helper methods
  private def identifyPhase(data: SimulationData): String = {
    val finalBurntFraction = data.burntFractions.last
    val maxPercolation = data.percolationIndicators.max
    
    if (finalBurntFraction < 0.1 && maxPercolation < 0.1) "Sub-critical"
    else if (finalBurntFraction > 0.5 && maxPercolation > 0.9) "Super-critical"
    else "Critical"
  }
  
  private def calculateOrderParameters(data: SimulationData): OrderParameterStats = {
    OrderParameterStats(
      meanBurntFraction = data.burntFractions.sum / data.burntFractions.length,
      maxClusterSize = data.percolationIndicators.max,
      fluctuations = calculateStandardDeviation(data.burntFractions)
    )
  }
  
  private def findCriticalPoints(data: SimulationData): List[Double] = {
    // Simplified critical point detection
    val threshold = 0.5
    data.percolationIndicators.zipWithIndex
      .filter(_._1 > threshold)
      .map(_._2.toDouble / data.timeSteps.length)
      .take(1)
  }
  
  private def computeBasicStatistics(data: SimulationData): BasicStats = {
    BasicStats(
      meanBurntFraction = data.burntFractions.sum / data.burntFractions.length,
      fireFrequency = data.activeFires.count(_ > 0).toDouble / data.timeSteps.length,
      maxBurntFraction = data.burntFractions.max,
      totalSteps = data.timeSteps.length
    )
  }
  
  private def analyzeTrends(data: SimulationData): TrendAnalysis = {
    // Simple linear regression for trend
    val n = data.timeSteps.length
    val xMean = data.timeSteps.sum.toDouble / n
    val yMean = data.burntFractions.sum / n
    
    val slope = if (n > 1) {
      val num = data.timeSteps.zip(data.burntFractions).map { case (x, y) =>
        (x - xMean) * (y - yMean)
      }.sum
      val den = data.timeSteps.map(x => Math.pow(x - xMean, 2)).sum
      if (den != 0) num / den else 0.0
    } else 0.0
    
    TrendAnalysis(
      burnRateTrend = slope,
      isStationary = Math.abs(slope) < 0.001
    )
  }
  
  private def computeTemporalCorrelations(data: SimulationData): CorrelationResults = {
    // Simplified autocorrelation calculation
    val autocorr = (1 to Math.min(50, data.burntFractions.length / 2)).map { lag =>
      calculateAutocorrelation(data.burntFractions, lag)
    }
    
    val autocorrTime = autocorr.indexWhere(_ < 0.1) match {
      case -1 => autocorr.length.toDouble
      case idx => idx.toDouble
    }
    
    CorrelationResults(
      autocorrelationTime = autocorrTime,
      correlationFunction = autocorr.toList
    )
  }
  
  private def analyzeFluctuations(data: SimulationData): List[Double] = {
    // Rolling window standard deviation
    val windowSize = 10
    data.burntFractions.sliding(windowSize).map { window =>
      calculateStandardDeviation(window.toList)
    }.toList
  }
  
  private def findSusceptibilityPeaks(fluctuations: List[Double]): List[Int] = {
    // Find local maxima in fluctuations
    fluctuations.zipWithIndex.sliding(3).collect {
      case Seq((a, _), (b, idx), (c, _)) if b > a && b > c => idx
    }.toList
  }
  
  private def calculateStandardDeviation(values: List[Double]): Double = {
    val mean = values.sum / values.length
    Math.sqrt(values.map(v => Math.pow(v - mean, 2)).sum / values.length)
  }
  
  private def calculateAutocorrelation(series: List[Double], lag: Int): Double = {
    val n = series.length - lag
    if (n <= 0) return 0.0
    
    val mean = series.sum / series.length
    val variance = series.map(v => Math.pow(v - mean, 2)).sum / series.length
    
    if (variance == 0) return 0.0
    
    val covariance = (0 until n).map { i =>
      (series(i) - mean) * (series(i + lag) - mean)
    }.sum / n
    
    covariance / variance
  }
  
  private def writeReport(path: Path, content: String): F[Unit] = 
    Sync[F].delay {
      Files.write(path, content.getBytes("UTF-8"))
      ()
    }
  
  // Report generation methods
  private def generatePhaseTransitionReport(
    phase: String,
    orderParams: OrderParameterStats,
    criticalPoints: List[Double]
  ): String = {
    s"""Phase Transition Analysis Report
    |================================
    |
    |Phase: $phase
    |
    |Order Parameters:
    |  Mean burnt fraction: ${f"${orderParams.meanBurntFraction}%.3f"}
    |  Maximum cluster size: ${f"${orderParams.maxClusterSize}%.3f"}
    |  Fluctuations (std dev): ${f"${orderParams.fluctuations}%.3f"}
    |
    |Critical Points:
    |  ${if (criticalPoints.isEmpty) "No critical points detected" 
         else criticalPoints.map(cp => f"t = ${cp}%.3f").mkString(", ")}
    |
    |Interpretation:
    |  ${interpretPhase(phase, orderParams)}
    """.stripMargin
  }
  
  private def interpretPhase(phase: String, orderParams: OrderParameterStats): String = phase match {
    case "Sub-critical" => 
      "The system is in a sub-critical state where fires remain localized and do not spread extensively."
    case "Critical" => 
      "The system exhibits critical behavior with scale-free fire clusters and power-law distributions."
    case "Super-critical" => 
      "The system is in a super-critical state where fires can span the entire system."
    case _ => 
      "Phase behavior could not be clearly determined."
  }
  
  private def generateStatisticalReport(stats: BasicStats, trends: TrendAnalysis): String = {
    s"""Statistical Analysis Report
    |==========================
    |
    |Basic Statistics:
    |  Total simulation steps: ${stats.totalSteps}
    |  Mean burnt fraction: ${f"${stats.meanBurntFraction}%.3f"}
    |  Maximum burnt fraction: ${f"${stats.maxBurntFraction}%.3f"}
    |  Fire frequency: ${f"${stats.fireFrequency}%.2f"} fires/step
    |
    |Trend Analysis:
    |  Burn rate trend: ${f"${trends.burnRateTrend}%.6f"}/step
    |  Stationary: ${if (trends.isStationary) "Yes" else "No"}
    |
    |Interpretation:
    |  ${interpretTrends(trends)}
    """.stripMargin
  }
  
  private def interpretTrends(trends: TrendAnalysis): String = {
    if (trends.isStationary) {
      "The system has reached a stationary state with stable fire dynamics."
    } else if (trends.burnRateTrend > 0) {
      "The fire is spreading at an increasing rate, indicating super-critical behavior."
    } else {
      "The fire is decaying over time, indicating sub-critical behavior."
    }
  }
  
  private def generateCorrelationReport(correlations: CorrelationResults): String = {
    s"""Correlation Analysis Report
    |===========================
    |
    |Temporal Correlations:
    |  Autocorrelation time: ${f"${correlations.autocorrelationTime}%.1f"} steps
    |  
    |Autocorrelation Function (first 10 lags):
    |${correlations.correlationFunction.take(10).zipWithIndex.map { case (c, i) =>
        f"  Lag ${i + 1}%2d: ${c}%.3f"
      }.mkString("\n")}
    |
    |Interpretation:
    |  ${interpretCorrelations(correlations)}
    """.stripMargin
  }
  
  private def interpretCorrelations(correlations: CorrelationResults): String = {
    if (correlations.autocorrelationTime < 5) {
      "The system has short memory, indicating rapid decorrelation of fire states."
    } else if (correlations.autocorrelationTime < 20) {
      "The system shows moderate temporal correlations typical of critical dynamics."
    } else {
      "The system exhibits long-range temporal correlations, suggesting slow dynamics or critical slowing down."
    }
  }
  
  private def generateCriticalPointReport(criticalRegions: List[Int]): String = {
    s"""Critical Point Analysis Report
    |==============================
    |
    |Critical Regions Detected: ${criticalRegions.length}
    |
    |${if (criticalRegions.isEmpty) {
        "No clear critical points were identified in the data."
      } else {
        s"""Critical regions found at time steps:
        |${criticalRegions.map(t => s"  - Step $t").mkString("\n")}
        |
        |These regions show enhanced fluctuations characteristic of critical behavior.""".stripMargin
      }}
    |
    |Note: For more accurate critical point detection, ensemble averaging over multiple 
    |simulation runs is recommended.
    """.stripMargin
  }
}

// Data classes for analysis
case class SimulationData(
  timeSteps: List[Int],
  burntFractions: List[Double],
  activeFires: List[Int],
  percolationIndicators: List[Double],
  metadata: Map[String, String]
)

case class OrderParameterStats(
  meanBurntFraction: Double,
  maxClusterSize: Double,
  fluctuations: Double
)

case class BasicStats(
  meanBurntFraction: Double,
  fireFrequency: Double,
  maxBurntFraction: Double,
  totalSteps: Int
)

case class TrendAnalysis(
  burnRateTrend: Double,
  isStationary: Boolean
)

case class CorrelationResults(
  autocorrelationTime: Double,
  correlationFunction: List[Double]
)