package app

import cats.effect._
import cats.effect.std.Console
import cats.effect.unsafe.implicits.global
import cats.implicits._
import fs2._
import cli._
import models._
import simulation._
import simulation.rules._
import _root_.io.swiss.{SwissDataModels, CoordinateSystems}
import _root_.io.importers.{SwissDataImporter, RegionData}
import _root_.io.exporters._
import _root_.io.geodata._
import analysis._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.http4s.ember.client.EmberClientBuilder
import java.nio.file.{Files, Paths}

class SimulationRunner[F[_]: Async: Console](
  config: AppConfig,
  progressReporter: ProgressReporter[F]
) {
  
  implicit def logger: Logger[F] = Slf4jLogger.getLogger[F]
  
  def run(command: RunCommand): F[ExitCode] = {
    for {
      _ <- logger.info(s"Starting simulation for region ${command.region}")
      _ <- Console[F].println(s"\n🌲 Forest Fire Simulator - ${command.region.toUpperCase} Region")
      _ <- Console[F].println("=" * 60)
      
      // Create output directory
      _ <- createOutputDirectory(command.output)
      
      // Determine if we're using a region scenario or legacy region
      (terrainData, baseClimate, regionInfo) <- RegionScenarios.byId(command.region) match {
        case Some(regionScenario) =>
          // Using new region scenario with optional real data
          for {
            scale <- Sync[F].fromOption(
              ScaleFactor.fromString(command.scale),
              new IllegalArgumentException(s"Invalid scale: ${command.scale}")
            )
            _ <- Console[F].println(s"\n📍 Using region scenario: ${regionScenario.name}")
            _ <- Console[F].println(RegionScenarios.printScenarioInfo(regionScenario, scale))
            
            result <- if (command.useRealData) {
              // Use real geodata
              Console[F].println("\n🌐 Fetching real geodata from Swiss federal services...") *>
              EmberClientBuilder.default[F].build.use { httpClient =>
                val importer = new SwissDataImporter[F](httpClient)
                for {
                  setup <- importer.createRegionSimulation(
                    scenario = regionScenario,
                    scale = scale,
                    useRealData = true,
                    climateScenario = command.scenario,
                    year = command.year
                  )
                } yield (setup.regionData.terrain, setup.baseClimate, s"${regionScenario.name} (${scale.name} scale, real data)")
              }
            } else {
              // Generate synthetic terrain
              for {
                _ <- Console[F].println("\n🎲 Generating synthetic terrain...")
                syntheticTerrain <- Sync[F].delay {
                  val (width, height) = regionScenario.getGridSize(scale)
                  terrain.TerrainGenerator.generateTerrain(
                    width = width,
                    height = height,
                    seed = (regionScenario.bounds.min.east + regionScenario.bounds.min.north).toLong
                  )
                }
                climate = Climate(
                  season = Season.Summer,
                  wind = Wind(
                    direction = if (regionScenario.characteristics.foehnFrequency > 0.1) 135.0 else 225.0,
                    speed = if (regionScenario.characteristics.foehnFrequency > 0.1) 15.0 else 10.0
                  ),
                  humidity = if (regionScenario.characteristics.foehnFrequency > 0.1) 0.35 else 0.5,
                  precipitation = 0.0
                )
              } yield (syntheticTerrain, climate, s"${regionScenario.name} (${scale.name} scale, synthetic data)")
            }
          } yield result
          
        case None =>
          // Legacy region support
          for {
            regionConfig <- Sync[F].fromOption(
              config.regions.get(command.region),
              new IllegalArgumentException(s"Unknown region: ${command.region}")
            )
            _ <- Console[F].println("\n📍 Generating terrain data...")
            legacyTerrain <- Sync[F].delay {
              terrain.TerrainGenerator.generateTerrain(
                width = regionConfig.sizeKm * 1000 / regionConfig.resolutionM,
                height = regionConfig.sizeKm * 1000 / regionConfig.resolutionM,
                seed = regionConfig.centerLat.toLong + regionConfig.centerLon.toLong
              )
            }
            baseClimate = Climate(
              season = Season.Summer,
              wind = Wind(direction = Math.PI * 1.5, speed = 5.0),
              humidity = 0.5,
              precipitation = 0.0
            )
          } yield (legacyTerrain, baseClimate, s"${command.region} (legacy)")
      }
      terrain = terrainData
      
      // Apply climate scenario
      _ <- Console[F].println(s"\n🌡️  Applying climate scenario: ${command.scenario} (${command.year})")
      climate <- applyClimateScenario(baseClimate, command.scenario, command.year)
      
      // Initialize simulation
      _ <- Console[F].println("\n🔥 Initializing simulation...")
      initialState <- initializeSimulation(terrain, climate)
      
      // Create rule engine
      ruleEngine = createRuleEngine()
      
      // Create simulation engine
      ioEngine = new IOSimulationEngine(ruleEngine = Some(ruleEngine))
      simulationConfig = SimulationConfig(
        maxSteps = command.steps,
        saveInterval = command.steps / 100, // Save 100 snapshots
        adaptiveTimeStep = config.simulation.adaptiveTimeStep,
        boundaryCondition = parseBoundaryCondition(config.simulation.boundaryCondition)
      )
      
      // Run simulation with progress reporting
      _ <- Console[F].println(s"\n⚙️  Running simulation (${command.steps} steps)...")
      _ <- progressReporter.start(command.steps.toLong, "Simulating")
      
      // Run IO simulation and convert to F
      states <- Sync[F].delay {
        var collectedStates = List.empty[SimulationState]
        ioEngine.run(initialState, command.steps, simulationConfig)
          .evalTap { state =>
            IO {
              collectedStates = collectedStates :+ state
            }
          }
          .compile
          .drain
          .unsafeRunSync()
        collectedStates
      }
      
      _ <- progressReporter.finish("Simulation complete!")
      
      // Get final state
      finalState <- Sync[F].fromOption(
        states.lastOption,
        new RuntimeException("No simulation states generated")
      )
      
      // Display summary
      _ <- Console[F].println(ConsoleFormatter.formatSummary(finalState))
      _ <- Console[F].println(s"\nRegion: ${regionInfo}")
      
      // Perform basic analysis
      _ <- Console[F].println("\n📊 Analyzing results...")
      analysis <- performBasicAnalysis(states)
      _ <- Console[F].println(ConsoleFormatter.formatPhaseAnalysis(analysis))
      
      // Export results
      _ <- Console[F].println(s"\n💾 Exporting results to ${command.output}/")
      _ <- Sync[F].delay {
        SimpleCSVExporter.exportSimulationResults(states, command.output.resolve("simulation_results.csv")).unsafeRunSync()
        SimpleCSVExporter.exportGridSnapshot(finalState.grid, command.output.resolve("final_state.csv")).unsafeRunSync()
        // Export phase data in a simplified format
        val phaseData = List((
          finalState.metrics.treeDensity,
          analysis.phase,
          Map("burnt_fraction" -> analysis.orderParameters.burntFraction,
              "percolation" -> analysis.orderParameters.percolationIndicator)
        ))
        SimpleCSVExporter.exportPhaseData(phaseData, command.output.resolve("phase_analysis.csv")).unsafeRunSync()
      }
      
      // Also export as JSON for web viewer
      _ <- Sync[F].delay {
        JSONExporter.exportSimulation(
          states,
          command.output.resolve("simulation.json"),
          deltaCompression = true,
          frameInterval = Math.max(1, command.steps / 100)
        ).unsafeRunSync()
      }
      
      _ <- Console[F].println(s"\n✅ Simulation complete! Results saved to ${command.output}/")
      _ <- Console[F].println("\nFiles created:")
      _ <- Console[F].println("  - simulation_results.csv: Time series data")
      _ <- Console[F].println("  - final_state.csv: Final grid state")
      _ <- Console[F].println("  - phase_analysis.csv: Phase transition analysis")
      _ <- Console[F].println("  - simulation.json: Data for web viewer")
      
      // Show example commands for different scales if using scenarios
      _ <- RegionScenarios.byId(command.region) match {
        case Some(scenario) =>
          Console[F].println("\n💡 Try different scales:") *>
          Console[F].println(s"  sbt \"run run --region ${scenario.id} --scale half\"") *>
          Console[F].println(s"  sbt \"run run --region ${scenario.id} --scale quarter --real-data\"")
        case None => Sync[F].unit
      }
      
    } yield ExitCode.Success
  }.handleErrorWith { error =>
    for {
      _ <- logger.error(error)("Simulation failed")
      _ <- Console[F].errorln(ConsoleFormatter.formatError(error))
    } yield ExitCode.Error
  }
  
  private def createOutputDirectory(path: java.nio.file.Path): F[Unit] = 
    Sync[F].delay(Files.createDirectories(path))
  
  private def applyClimateScenario(
    baseClimate: Climate,
    scenario: String,
    year: Int
  ): F[Climate] = {
    scenario match {
      case ClimateScenarios.Baseline => Sync[F].pure(baseClimate)
      case _ =>
        Sync[F].delay {
          val projectionScenario = scenario match {
            case ClimateScenarios.RCP26 => SwissDataModels.RCP26
            case ClimateScenarios.RCP45 => SwissDataModels.RCP45
            case ClimateScenarios.RCP85 => SwissDataModels.RCP85
            case _ => SwissDataModels.RCP26 // Default fallback
          }
          
          // Simplified climate projection
          val tempIncrease = projectionScenario match {
            case SwissDataModels.RCP26 => (year - 2020) * 0.01
            case SwissDataModels.RCP45 => (year - 2020) * 0.02
            case SwissDataModels.RCP85 => (year - 2020) * 0.03
          }
          val humidityDecrease = tempIncrease * 0.1
          
          baseClimate.copy(
            humidity = Math.max(0.1, baseClimate.humidity - humidityDecrease),
            wind = baseClimate.wind.copy(speed = baseClimate.wind.speed * (1 + tempIncrease * 0.05))
          )
        }
    }
  }
  
  private def initializeSimulation(
    terrain: Terrain,
    climate: Climate
  ): F[SimulationState] = Sync[F].delay {
    val grid = GridInitializer.initializeGrid(terrain, climate)
    val initialMetrics = MetricsCollector.collectMetrics(
      SimulationState(
        grid = grid,
        climate = climate,
        terrain = terrain,
        timeStep = 0,
        elapsedTime = 0.0,
        metrics = SimulationMetrics(0, 0, 0, 0.0, 0.0, 0.0, 0.0),
        eventLog = List.empty
      )
    )
    
    SimulationState(
      grid = grid,
      climate = climate,
      terrain = terrain,
      timeStep = 0,
      elapsedTime = 0.0,
      metrics = initialMetrics,
      eventLog = List.empty
    )
  }
  
  private def createRuleEngine(): RuleEngine[IO] = {
    val fireDynamics = config.simulation.fireDynamics
    
    new RuleEngine[IO](
      ignitionRules = List(
        IgnitionRules.SparkIgnition[IO](fireDynamics.baseIgnitionProbability),
        IgnitionRules.NeighborIgnition[IO](),
        IgnitionRules.EmberIgnition[IO](maxDistance = 3)
      ),
      burningRules = List(
        BurningRules.IntensityEvolution[IO](),
        BurningRules.FuelConsumption[IO](),
        BurningRules.HeatGeneration[IO](),
        BurningRules.PreHeating[IO]()
      ),
      extinctionRules = List(
        ExtinctionRules.FuelDepletion[IO](),
        ExtinctionRules.MoistureSuppression[IO](),
        ExtinctionRules.TemperatureDecay[IO](),
        ExtinctionRules.NeighborIsolation[IO]()
      ),
      recoveryRules = List(
        RecoveryRules.NaturalRegrowth[IO](regrowthRate = 0.001),
        RecoveryRules.SeasonalGrowth[IO](),
        RecoveryRules.SeedDispersion[IO](),
        RecoveryRules.VegetationSuccession[IO]()
      ),
      interventionRules = List.empty
    )
  }
  
  private def parseBoundaryCondition(condition: String): BoundaryCondition = 
    condition.toLowerCase match {
      case "periodic" => PeriodicBoundary
      case "reflective" => ReflectiveBoundary
      case "fixed" => FixedBoundary(Empty)
      case _ => AbsorbingBoundary
    }
  
  private def performBasicAnalysis(states: List[SimulationState]): F[BasicPhaseAnalysisResult] = {
    Sync[F].delay {
      // Simple phase identification based on final state
      val finalState = states.last
      val orderParams = OrderParametersData(
        burntFraction = OrderParameters.burntFraction(finalState),
        largestClusterRatio = OrderParameters.largestClusterRatio(finalState),
        percolationIndicator = OrderParameters.percolationIndicator(finalState),
        clusterDensity = OrderParameters.clusterDensity(finalState),
        averageClusterSize = OrderParameters.averageClusterSize(finalState),
        treeDensity = finalState.metrics.treeDensity
      )
      val phase = PhaseIdentifier.identifyPhase(finalState)
      
      // Estimate critical points from the simulation
      val criticalPoints = if (orderParams.percolationIndicator > 0.1 && orderParams.percolationIndicator < 0.9) {
        List(SimpleCriticalPoint("tree_density", orderParams.treeDensity, 0.01))
      } else List.empty
      
      BasicPhaseAnalysisResult(
        phase = phase,
        orderParameters = orderParams,
        criticalPoints = criticalPoints,
        universalityClass = if (phase == Critical) Some("2D Percolation") else None
      )
    }
  }
}

// Simplified data types for basic analysis
case class BasicPhaseAnalysisResult(
  phase: Phase,
  orderParameters: OrderParametersData,
  criticalPoints: List[SimpleCriticalPoint],
  universalityClass: Option[String]
)

case class SimpleCriticalPoint(
  parameter: String,
  value: Double,
  uncertainty: Double
)

case class OrderParametersData(
  burntFraction: Double,
  largestClusterRatio: Double,
  percolationIndicator: Double,
  clusterDensity: Double,
  averageClusterSize: Double,
  treeDensity: Double
)