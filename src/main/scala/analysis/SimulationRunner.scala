package analysis

import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.implicits._
import cats.Parallel
import fs2.Stream
import models._
import simulation._
import simulation.rules._

/**
 * Configuration for analysis runs
 */
case class AnalysisConfig(
  baseConfig: SimulationConfig,
  ensembleSize: Int = 10,
  maxTime: Double = 100.0,
  measurementInterval: Double = 1.0,
  warmupTime: Double = 0.0,
  ruleConfig: RuleConfig = RuleConfig()
)

/**
 * Results from a single simulation run
 */
case class SimulationResult(
  finalState: SimulationState,
  timeSeries: List[SimulationState],
  orderParameters: Map[String, Double],
  phase: Phase,
  runTime: Double
)

/**
 * Results from ensemble of simulations
 */
case class EnsembleResults(
  results: List[SimulationResult],
  averageOrderParameters: Map[String, Double],
  standardDeviations: Map[String, Double],
  phase: Phase
)

/**
 * Data point for parameter sweep
 */
case class ParameterPoint(
  parameter: PhaseParameter,
  value: Double,
  ensemble: EnsembleResults
)

/**
 * Runs batches of simulations for phase analysis
 */
class SimulationRunner[F[_]: Async: Parallel](
  engineFactory: RuleConfig => IOSimulationEngine = config => {
    val ruleEngine = RuleEngine.default[IO](config)
    IOSimulationEngine.withRuleEngine(ruleEngine)
  }
) {
  
  /**
   * Run a single simulation to completion
   */
  def runSimulation(
    initialState: SimulationState,
    config: AnalysisConfig
  ): F[SimulationResult] = {
    val engine = engineFactory(config.ruleConfig)
    
    Async[F].delay {
      val startTime = System.currentTimeMillis()
      
      // Run simulation collecting states at measurement intervals
      val states = engine.runAdaptive(initialState, config.maxTime, config.baseConfig)
        .filter(state => 
          state.elapsedTime >= config.warmupTime &&
          (state.elapsedTime % config.measurementInterval).abs < config.baseConfig.minTimeStep
        )
        .compile
        .toList
        .unsafeRunSync()
      
      val finalState = states.lastOption.getOrElse(initialState)
      val orderParams = OrderParameters.calculateAll(finalState)
      val phase = PhaseIdentifier.identifyPhase(finalState)
      val runTime = (System.currentTimeMillis() - startTime) / 1000.0
      
      SimulationResult(finalState, states, orderParams, phase, runTime)
    }
  }
  
  /**
   * Run ensemble of simulations with same parameters
   */
  def runEnsemble(
    baseState: SimulationState,
    config: AnalysisConfig
  ): F[EnsembleResults] = {
    // Create different initial conditions for each run
    val initialStates = (0 until config.ensembleSize).map { i =>
      // Add small random perturbations or different random seeds
      val modifiedGrid = if (i == 0) baseState.grid else {
        val random = new scala.util.Random(42 + i)
        // Randomly ignite a few cells
        val grid = baseState.grid
        val ignitions = (0 until 3).map { _ =>
          val x = random.nextInt(grid.width)
          val y = random.nextInt(grid.height)
          (x, y)
        }
        
        ignitions.foldLeft(grid) { case (g, (x, y)) =>
          g.get(x, y) match {
            case Some(cell) if cell.state == Tree =>
              g.updated(x, y, cell.copy(state = Burning))
            case _ => g
          }
        }
      }
      baseState.copy(grid = modifiedGrid)
    }.toList
    
    // Run simulations in parallel
    initialStates.parTraverse { state =>
      runSimulation(state, config)
    }.map { results =>
      // Calculate ensemble statistics
      val allOrderParams = results.map(_.orderParameters)
      val paramNames = allOrderParams.headOption.map(_.keys.toList).getOrElse(List.empty)
      
      val averages = paramNames.map { param =>
        val values = allOrderParams.map(_.getOrElse(param, 0.0))
        param -> (values.sum / values.length)
      }.toMap
      
      val stdDevs = paramNames.map { param =>
        val values = allOrderParams.map(_.getOrElse(param, 0.0))
        val avg = averages(param)
        val variance = values.map(v => math.pow(v - avg, 2)).sum / values.length
        param -> math.sqrt(variance)
      }.toMap
      
      val phase = PhaseIdentifier.identifyFromEnsemble(results.map(_.finalState))
      
      EnsembleResults(results, averages, stdDevs, phase)
    }
  }
  
  /**
   * Parameter sweep - vary one parameter over a range
   */
  def parameterSweep(
    parameter: PhaseParameter,
    values: List[Double],
    baseState: SimulationState,
    config: AnalysisConfig
  ): Stream[F, ParameterPoint] = {
    Stream.emits(values)
      .parEvalMap(config.ensembleSize.min(4)) { value =>
        // Apply parameter to base state
        val modifiedState = parameter.applyToState(baseState, value)
        
        // Update rule config if needed
        val modifiedConfig = parameter match {
          case SparkProbabilityParameter =>
            config.copy(ruleConfig = config.ruleConfig.copy(sparkProbability = value))
          case _ => config
        }
        
        // Run ensemble at this parameter value
        runEnsemble(modifiedState, modifiedConfig).map { ensemble =>
          ParameterPoint(parameter, value, ensemble)
        }
      }
  }
  
  /**
   * Two-parameter phase diagram
   */
  def phaseDiagram(
    param1: PhaseParameter,
    values1: List[Double],
    param2: PhaseParameter,
    values2: List[Double],
    baseState: SimulationState,
    config: AnalysisConfig
  ): F[List[List[ParameterPoint]]] = {
    // Create grid of parameter combinations
    val parameterGrid = for {
      v1 <- values1
      v2 <- values2
    } yield (v1, v2)
    
    // Run simulations for each combination
    parameterGrid.grouped(values2.length).toList.parTraverse { row =>
      row.parTraverse { case (v1, v2) =>
        // Apply both parameters
        val state1 = param1.applyToState(baseState, v1)
        val state2 = param2.applyToState(state1, v2)
        
        // Update config if needed
        var modConfig = config
        if (param1 == SparkProbabilityParameter) {
          modConfig = modConfig.copy(ruleConfig = modConfig.ruleConfig.copy(sparkProbability = v1))
        }
        if (param2 == SparkProbabilityParameter) {
          modConfig = modConfig.copy(ruleConfig = modConfig.ruleConfig.copy(sparkProbability = v2))
        }
        
        runEnsemble(state2, modConfig).map { ensemble =>
          // Store both parameter values in the result
          ParameterPoint(param1, v1, ensemble)
        }
      }
    }
  }
  
  /**
   * Time evolution study - track how system evolves
   */
  def timeEvolution(
    initialState: SimulationState,
    config: AnalysisConfig,
    measurementTimes: List[Double]
  ): F[List[(Double, Map[String, Double])]] = {
    val engine = engineFactory(config.ruleConfig)
    
    Async[F].delay {
      var currentState = initialState
      val results = scala.collection.mutable.ListBuffer[(Double, Map[String, Double])]()
      
      for (targetTime <- measurementTimes.sorted) {
        // Run until target time
        val states = engine.runAdaptive(currentState, targetTime - currentState.elapsedTime, config.baseConfig)
          .compile
          .toList
          .unsafeRunSync()
        
        currentState = states.lastOption.getOrElse(currentState)
        val orderParams = OrderParameters.calculateAll(currentState)
        results += ((targetTime, orderParams))
      }
      
      results.toList
    }
  }
}