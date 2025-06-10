package io.exporters

import models._
import simulation._
import analysis._
import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Paths}
import scala.io.Source

class SimpleExportersSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  
  val testOutputDir = Paths.get("target", "test-output")
  
  // Ensure test directory exists before each test
  def withTestDirectory[A](test: => IO[A]): IO[A] = {
    for {
      _ <- IO(Files.createDirectories(testOutputDir))
      result <- test
      _ <- IO {
        // Clean up test files
        if (Files.exists(testOutputDir)) {
          Files.walk(testOutputDir)
            .sorted(java.util.Comparator.reverseOrder())
            .forEach(Files.deleteIfExists(_))
        }
      }
    } yield result
  }
  
  "SimpleCSVExporter.exportSimulationResults" should "create valid CSV file" in {
    val states = List(
      createTestState(0.0, 5, 0),
      createTestState(1.0, 10, 5),
      createTestState(2.0, 8, 15)
    )
    
    val outputPath = testOutputDir.resolve("test_results.csv")
    
    withTestDirectory {
      SimpleCSVExporter.exportSimulationResults(states, outputPath).flatMap { _ => IO {
      Files.exists(outputPath) should be (true)
      
      val lines = Source.fromFile(outputPath.toFile).getLines().toList
      lines.head should be ("time,active_fires,burnt_area,largest_cluster,tree_density,percolation_indicator")
      lines.length should be (4) // Header + 3 data rows
      
      // Check first data row
      val firstData = lines(1).split(",")
      firstData(0) should be ("0.00")
      firstData(1) should be ("5")
      firstData(2) should be ("0")
      }}
    }.assertNoException
  }
  
  "SimpleCSVExporter.exportPhaseData" should "export phase transition data" in {
    val phaseData = List(
      (0.3, SubCritical, Map("burntFraction" -> 0.1, "percolationIndicator" -> 0.0)),
      (0.5, Critical, Map("burntFraction" -> 0.4, "percolationIndicator" -> 0.3)),
      (0.7, SuperCritical, Map("burntFraction" -> 0.8, "percolationIndicator" -> 1.0))
    )
    
    val outputPath = testOutputDir.resolve("phase_data.csv")
    
    withTestDirectory {
      SimpleCSVExporter.exportPhaseData(phaseData, outputPath).flatMap { _ => IO {
        Files.exists(outputPath) should be (true)
        
        val lines = Source.fromFile(outputPath.toFile).getLines().toList
        lines.head should be ("parameter_value,phase,burnt_fraction,percolation")
        lines.length should be (4)
        
        // Check critical point data
        val criticalData = lines(2).split(",")
        criticalData(0) should be ("0.500")
        criticalData(1) should be ("Critical")
        criticalData(2) should be ("0.400")
      }}
    }.assertNoException
  }
  
  "SimpleCSVExporter.exportGridSnapshot" should "export grid state" in {
    val cells = Vector.tabulate(3, 3) { (y, x) =>
      Cell(
        position = Position(x, y),
        state = if (x == 1 && y == 1) Burning else Tree,
        elevation = 600.0 + x * 10.0 + y * 5.0,
        vegetationType = VegetationType.DenseForest,
        moisture = 0.5,
        temperature = 20.0
      )
    }
    val grid = Grid(cells, 3, 3)
    
    val outputPath = testOutputDir.resolve("grid_snapshot.csv")
    
    withTestDirectory {
      SimpleCSVExporter.exportGridSnapshot(grid, outputPath).flatMap { _ => IO {
        Files.exists(outputPath) should be (true)
        
        val lines = Source.fromFile(outputPath.toFile).getLines().toList
        lines.head should be ("x,y,state,elevation,vegetation,moisture,temperature")
        lines.length should be (10) // Header + 9 cells
        
        // Check burning cell
        val burningCell = lines.find(_.contains("Burning")).get
        burningCell should include ("1,1,Burning")
      }}
    }.assertNoException
  }
  
  "SimpleCSVExporter.exportClimateScenarios" should "export climate data" in {
    val scenarios: Map[(io.swiss.SwissDataModels.ClimateScenario, Int), Climate] = Map(
      (io.swiss.SwissDataModels.RCP26: io.swiss.SwissDataModels.ClimateScenario, 2050) -> Climate(
        Season.Summer, Wind(180, 10.0), 0.45, 0.0
      ),
      (io.swiss.SwissDataModels.RCP45: io.swiss.SwissDataModels.ClimateScenario, 2050) -> Climate(
        Season.Summer, Wind(180, 12.0), 0.40, 0.0
      ),
      (io.swiss.SwissDataModels.RCP85: io.swiss.SwissDataModels.ClimateScenario, 2050) -> Climate(
        Season.Summer, Wind(180, 15.0), 0.35, 0.0
      )
    )
    
    val outputPath = testOutputDir.resolve("climate_scenarios.csv")
    
    withTestDirectory {
      SimpleCSVExporter.exportClimateScenarios(scenarios, outputPath).flatMap { _ => IO {
        Files.exists(outputPath) should be (true)
        
        val lines = Source.fromFile(outputPath.toFile).getLines().toList
        lines.head should be ("scenario,year,humidity,wind_speed,wind_direction")
        lines.length should be (4) // Header + 3 scenarios
        
        // Verify all scenarios are present
        val dataLines = lines.tail
        dataLines.exists(_.startsWith("RCP2.6,2050")) should be (true)
        dataLines.exists(_.startsWith("RCP4.5,2050")) should be (true)
        dataLines.exists(_.startsWith("RCP8.5,2050")) should be (true)
      }}
    }.assertNoException
  }
  
  // Helper method to create test states
  private def createTestState(time: Double, activeFires: Int, burntArea: Int): SimulationState = {
    val cells = Vector.fill(10, 10)(
      Cell(Position(0, 0), Tree, 600.0, VegetationType.DenseForest, 0.5, 20.0)
    )
    
    SimulationState(
      grid = Grid(cells, 10, 10),
      climate = Climate(Season.Summer, Wind(0, 10.0), 0.5, 0.0),
      terrain = Terrain(Vector.fill(10, 10)(600.0), 10, 10),
      timeStep = 0.1,
      elapsedTime = time,
      metrics = SimulationMetrics(
        activeFires = activeFires,
        totalBurntArea = burntArea,
        largestFireClusterSize = burntArea / 2,
        averageFireIntensity = 100.0,
        percolationIndicator = if (burntArea > 50) 1.0 else 0.0,
        treeDensity = 0.8,
        averageMoisture = 0.5
      ),
      eventLog = List.empty
    )
  }
}