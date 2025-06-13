package app

import cats.effect._
import cats.effect.std.Console
import cats.implicits._
import io.geodata._
import cli._

object RegionScenarioDemo {
  
  def run[F[_]: Async: Console]: F[Unit] = {
    for {
      _ <- Console[F].println("\n🏔️  Available Swiss Region Scenarios")
      _ <- Console[F].println("=" * 80)
      
      // Show all available scenarios
      _ <- RegionScenarios.all.traverse { scenario =>
        for {
          _ <- Console[F].println(s"\n${scenario.id}: ${scenario.name}")
          _ <- Console[F].println("-" * scenario.name.length)
          _ <- Console[F].println(scenario.description)
          _ <- Console[F].println(s"Area: ${((scenario.bounds.max.east - scenario.bounds.min.east) * (scenario.bounds.max.north - scenario.bounds.min.north) / 1_000_000).formatted("%.1f")} km²")
          _ <- Console[F].println(s"Dominant vegetation: ${scenario.characteristics.dominantVegetation}")
          _ <- Console[F].println(s"Fire return interval: ${scenario.characteristics.fireReturnInterval} years")
          
          // Show grid sizes for different scales
          _ <- Console[F].println("\nGrid sizes by scale:")
          _ <- ScaleFactor.all.traverse { scale =>
            val (width, height) = scenario.getGridSize(scale)
            val cells = scenario.getCellCount(scale)
            val resolution = scenario.getResolution(scale)
            Console[F].println(s"  ${scale.name.padTo(8, ' ')}: ${width}×${height} = ${cells} cells (${resolution}m resolution)")
          }
        } yield ()
      }
      
      _ <- Console[F].println("\n" + "=" * 80)
      _ <- Console[F].println("\n📌 Example commands:")
      _ <- Console[F].println("\n# Quick test with small grid (2×2 km, 80×80 cells):")
      _ <- Console[F].println("sbt \"run run --region test --scale full --steps 500\"")
      
      _ <- Console[F].println("\n# Finges forest at different scales:")
      _ <- Console[F].println("sbt \"run run --region finges --scale full\"     # 400×240 cells (25m)")
      _ <- Console[F].println("sbt \"run run --region finges --scale half\"     # 200×120 cells (50m)")
      _ <- Console[F].println("sbt \"run run --region finges --scale quarter\"  # 100×60 cells (100m)")
      
      _ <- Console[F].println("\n# Using real geodata (requires internet connection):")
      _ <- Console[F].println("sbt \"run run --region finges --scale quarter --real-data\"")
      
      _ <- Console[F].println("\n# Climate scenarios:")
      _ <- Console[F].println("sbt \"run run --region finges --scenario rcp45 --year 2050\"")
      _ <- Console[F].println("sbt \"run run --region finges --scenario rcp85 --year 2100 --real-data\"")
      
      _ <- Console[F].println("\n# Steep terrain study:")
      _ <- Console[F].println("sbt \"run run --region visp-stalden --scale half\"")
      
      _ <- Console[F].println("\n💡 Tips:")
      _ <- Console[F].println("- Start with 'test' region for quick experiments")
      _ <- Console[F].println("- Use 'quarter' scale for faster runs")
      _ <- Console[F].println("- Add --real-data flag to use actual Swiss geodata")
      _ <- Console[F].println("- Combine with climate scenarios for future projections")
      
    } yield ()
  }
}