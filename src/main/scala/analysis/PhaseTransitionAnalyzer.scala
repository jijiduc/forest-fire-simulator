package analysis

import cats.effect._
import cats.implicits._
import cats.Parallel
import models._
import simulation._
import terrain.TerrainGenerator

/**
 * Results from phase transition analysis
 */
case class PhaseAnalysisResult(
  parameter: PhaseParameter,
  parameterRange: ParameterRange,
  dataPoints: List[ParameterPoint],
  criticalPoint: Option[CriticalPoint],
  criticalExponents: Option[CriticalExponents],
  phaseBoundaries: List[(Double, Phase)]
)

/**
 * Results from tree density analysis
 */
case class TreeDensityAnalysis(
  climate: Climate,
  criticalDensity: Double,
  confidence: Double,
  phaseData: List[(Double, Phase, Map[String, Double])],
  percolationThreshold: Double
)

/**
 * Results from moisture analysis
 */
case class MoistureAnalysis(
  treeDensity: Double,
  criticalMoisture: Double,
  confidence: Double,
  phaseData: List[(Double, Phase, Map[String, Double])]
)

/**
 * 2D phase diagram data
 */
case class PhaseDiagram(
  param1: PhaseParameter,
  param2: PhaseParameter,
  data: List[List[Phase]],
  criticalLine: List[(Double, Double)]
)

/**
 * Main analyzer for phase transitions in forest fire model
 */
class PhaseTransitionAnalyzer[F[_]: Async: Parallel](
  runner: SimulationRunner[F],
  gridSize: Int = 50,
  defaultConfig: AnalysisConfig = AnalysisConfig(
    baseConfig = SimulationConfig(
      adaptiveTimeStep = true,
      boundaryCondition = PeriodicBoundary
    ),
    ensembleSize = 10,
    maxTime = 50.0,
    measurementInterval = 1.0
  )
) {
  
  /**
   * Analyze tree density phase transition for a given climate
   */
  def analyzeTreeDensityTransition(
    climate: Climate,
    densityRange: ParameterRange = ParameterRange(0.0, 1.0, 21),
    terrain: Terrain = TerrainGenerator.generateTerrain(50, 50, 42L)
  ): F[TreeDensityAnalysis] = {
    // Create base state
    val baseGrid = GridInitializer.initializeGrid(terrain, climate, 42L)
    val baseState = SimulationState(
      grid = baseGrid,
      climate = climate,
      terrain = terrain,
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
    
    // Run parameter sweep
    val sweep = runner.parameterSweep(
      TreeDensityParameter,
      densityRange.values,
      baseState,
      defaultConfig
    ).compile.toList
    
    sweep.flatMap { dataPoints =>
      // Find critical point
      val criticalPoint = CriticalPointFinder.susceptibilityPeak(dataPoints)
      
      // Extract phase data
      val phaseData = dataPoints.map { point =>
        (
          point.value,
          point.ensemble.phase,
          point.ensemble.averageOrderParameters
        )
      }
      
      // Find percolation threshold (where percolation indicator jumps)
      val percolationData = dataPoints.map { point =>
        (point.value, point.ensemble.averageOrderParameters.getOrElse("percolationIndicator", 0.0))
      }
      
      val percolationThreshold = findPercolationThreshold(percolationData)
      
      Sync[F].pure(TreeDensityAnalysis(
        climate = climate,
        criticalDensity = criticalPoint.map(_.value).getOrElse(0.5),
        confidence = criticalPoint.map(_.confidence).getOrElse(0.0),
        phaseData = phaseData,
        percolationThreshold = percolationThreshold
      ))
    }
  }
  
  /**
   * Analyze moisture phase transition for fixed tree density
   */
  def analyzeMoistureTransition(
    treeDensity: Double,
    climate: Climate,
    moistureRange: ParameterRange = ParameterRange(0.0, 1.0, 21),
    terrain: Terrain = TerrainGenerator.generateTerrain(50, 50, 42L)
  ): F[MoistureAnalysis] = {
    // Create base state with specified tree density
    val baseGrid = GridInitializer.initializeGrid(terrain, climate, 42L)
    val modifiedGrid = TreeDensityParameter.applyToGrid(baseGrid, treeDensity, terrain, climate)
    
    val baseState = SimulationState(
      grid = modifiedGrid,
      climate = climate,
      terrain = terrain,
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
    
    // Run parameter sweep
    val sweep = runner.parameterSweep(
      MoistureParameter,
      moistureRange.values,
      baseState,
      defaultConfig
    ).compile.toList
    
    sweep.flatMap { dataPoints =>
      // Find critical point
      val criticalPoint = CriticalPointFinder.susceptibilityPeak(dataPoints)
      
      // Extract phase data
      val phaseData = dataPoints.map { point =>
        (
          point.value,
          point.ensemble.phase,
          point.ensemble.averageOrderParameters
        )
      }
      
      Sync[F].pure(MoistureAnalysis(
        treeDensity = treeDensity,
        criticalMoisture = criticalPoint.map(_.value).getOrElse(0.5),
        confidence = criticalPoint.map(_.confidence).getOrElse(0.0),
        phaseData = phaseData
      ))
    }
  }
  
  /**
   * Generate 2D phase diagram
   */
  def generatePhaseDiagram(
    param1: PhaseParameter,
    range1: ParameterRange,
    param2: PhaseParameter,
    range2: ParameterRange,
    climate: Climate,
    terrain: Terrain = TerrainGenerator.generateTerrain(50, 50, 42L)
  ): F[PhaseDiagram] = {
    val baseGrid = GridInitializer.initializeGrid(terrain, climate, 42L)
    val baseState = SimulationState(
      grid = baseGrid,
      climate = climate,
      terrain = terrain,
      timeStep = 0.1,
      elapsedTime = 0.0,
      metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
      eventLog = List.empty
    )
    
    // Run 2D parameter sweep
    runner.phaseDiagram(
      param1, range1.values,
      param2, range2.values,
      baseState,
      defaultConfig.copy(ensembleSize = 5) // Reduce ensemble size for 2D sweeps
    ).map { gridData =>
      // Extract phases
      val phases = gridData.map { row =>
        row.map(_.ensemble.phase)
      }
      
      // Find critical line (phase boundaries)
      val criticalLine = findCriticalLine(gridData, range1.values, range2.values)
      
      PhaseDiagram(param1, param2, phases, criticalLine)
    }
  }
  
  /**
   * Comprehensive phase analysis for a parameter
   */
  def analyzeParameter(
    parameter: PhaseParameter,
    range: ParameterRange,
    baseState: SimulationState,
    config: AnalysisConfig = defaultConfig
  ): F[PhaseAnalysisResult] = {
    // Coarse sweep first
    val coarseRange = ParameterRange(range.min, range.max, 11)
    
    runner.parameterSweep(parameter, coarseRange.values, baseState, config)
      .compile.toList.flatMap { coarseData =>
        
        // Find approximate critical region
        val criticalRegion = findCriticalRegion(coarseData)
        
        // Fine sweep around critical region
        val fineRange = criticalRegion match {
          case Some((low, high)) => ParameterRange(low, high, 21)
          case None => range
        }
        
        runner.parameterSweep(parameter, fineRange.values, baseState, config)
          .compile.toList.flatMap { fineData =>
            
            // Find critical point
            val criticalPoint = CriticalPointFinder.susceptibilityPeak(fineData)
            
            // Estimate critical exponents if critical point found
            val criticalExponents = criticalPoint.map { cp =>
              CriticalPointFinder.estimateCriticalExponents(fineData, cp.value)
            }
            
            // Identify phase boundaries
            val phaseBoundaries = identifyPhaseBoundaries(fineData)
            
            Sync[F].pure(PhaseAnalysisResult(
              parameter = parameter,
              parameterRange = range,
              dataPoints = coarseData ++ fineData,
              criticalPoint = criticalPoint,
              criticalExponents = criticalExponents,
              phaseBoundaries = phaseBoundaries
            ))
          }
      }
  }
  
  /**
   * Multi-parameter critical surface
   */
  def findCriticalSurface(
    parameters: List[PhaseParameter],
    climate: Climate
  ): F[List[(PhaseParameter, Double)]] = {
    // This would require more sophisticated optimization
    // For now, analyze each parameter independently
    parameters.traverse { param =>
      val range = param match {
        case TreeDensityParameter => ParameterRange(0.0, 1.0, 21)
        case MoistureParameter => ParameterRange(0.0, 1.0, 21)
        case WindSpeedParameter => ParameterRange(0.0, 20.0, 21)
        case TemperatureParameter => ParameterRange(-10.0, 10.0, 21)
        case SparkProbabilityParameter => ParameterRange(0.0, 0.01, 21)
      }
      
      analyzeTreeDensityTransition(climate, range).map { analysis =>
        (param, analysis.criticalDensity)
      }
    }
  }
  
  // Helper methods
  
  private def findPercolationThreshold(data: List[(Double, Double)]): Double = {
    // Find steepest increase in percolation indicator
    if (data.length < 2) return 0.5
    
    val derivatives = for (i <- 1 until data.length) yield {
      val (x1, y1) = data(i-1)
      val (x2, y2) = data(i)
      val derivative = if (x2 != x1) (y2 - y1) / (x2 - x1) else 0.0
      (x1, derivative)
    }
    
    derivatives.maxBy(_._2)._1
  }
  
  private def findCriticalRegion(data: List[ParameterPoint]): Option[(Double, Double)] = {
    // Find region with highest variance
    val variances = data.map { point =>
      val variance = point.ensemble.standardDeviations.values.sum / point.ensemble.standardDeviations.size
      (point.value, variance)
    }
    
    if (variances.isEmpty) return None
    
    val maxVariance = variances.maxBy(_._2)._2
    val threshold = maxVariance * 0.5
    
    val criticalPoints = variances.filter(_._2 > threshold).map(_._1)
    if (criticalPoints.nonEmpty) {
      Some((criticalPoints.min, criticalPoints.max))
    } else {
      None
    }
  }
  
  private def identifyPhaseBoundaries(data: List[ParameterPoint]): List[(Double, Phase)] = {
    if (data.isEmpty) return List.empty
    
    val phases = data.map(p => (p.value, p.ensemble.phase))
    val boundaries = scala.collection.mutable.ListBuffer[(Double, Phase)]()
    
    boundaries += phases.head
    
    for (i <- 1 until phases.length) {
      if (phases(i)._2 != phases(i-1)._2) {
        boundaries += phases(i)
      }
    }
    
    boundaries.toList
  }
  
  private def findCriticalLine(
    gridData: List[List[ParameterPoint]],
    values1: List[Double],
    values2: List[Double]
  ): List[(Double, Double)] = {
    val criticalPoints = scala.collection.mutable.ListBuffer[(Double, Double)]()
    
    for (i <- 0 until gridData.length - 1) {
      for (j <- 0 until gridData(i).length - 1) {
        val phase00 = gridData(i)(j).ensemble.phase
        val phase01 = gridData(i)(j+1).ensemble.phase
        val phase10 = gridData(i+1)(j).ensemble.phase
        
        // Check for phase boundary
        if (phase00 != phase01 || phase00 != phase10) {
          criticalPoints += ((values1(i), values2(j)))
        }
      }
    }
    
    criticalPoints.toList
  }
}