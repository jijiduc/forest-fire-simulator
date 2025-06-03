package terrain

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TerrainGeneratorSpec extends AnyFlatSpec with Matchers {
  
  "TerrainGenerator" should "generate terrain with correct dimensions" in {
    val width = 50
    val height = 40
    val terrain = TerrainGenerator.generateTerrain(width, height)
    
    terrain.width shouldBe width
    terrain.height shouldBe height
    terrain.elevationMap.length shouldBe height
    terrain.elevationMap.foreach(_.length shouldBe width)
  }
  
  it should "generate elevations within expected range" in {
    val terrain = TerrainGenerator.generateTerrain(100, 100)
    
    val allElevations = for {
      row <- terrain.elevationMap
      elevation <- row
    } yield elevation
    
    val minElevation = allElevations.min
    val maxElevation = allElevations.max
    
    minElevation should be >= 0.0
    maxElevation should be <= 4000.0
    
    // Should have reasonable variation
    (maxElevation - minElevation) should be > 1000.0
  }
  
  it should "generate consistent terrain with same seed" in {
    val seed = 12345L
    val terrain1 = TerrainGenerator.generateTerrain(50, 50, seed)
    val terrain2 = TerrainGenerator.generateTerrain(50, 50, seed)
    
    terrain1.elevationMap shouldBe terrain2.elevationMap
  }
  
  it should "generate different terrain with different seeds" in {
    val terrain1 = TerrainGenerator.generateTerrain(50, 50, 12345L)
    val terrain2 = TerrainGenerator.generateTerrain(50, 50, 54321L)
    
    terrain1.elevationMap should not be terrain2.elevationMap
  }
  
  it should "generate realistic terrain gradients" in {
    val terrain = TerrainGenerator.generateTerrain(50, 50)
    
    // Check average gradient is reasonable
    var totalGradient = 0.0
    var count = 0
    
    for {
      y <- 0 until terrain.height - 1
      x <- 0 until terrain.width - 1
    } {
      val current = terrain.elevationAt(x, y)
      val right = terrain.elevationAt(x + 1, y)
      val down = terrain.elevationAt(x, y + 1)
      
      totalGradient += math.abs(current - right)
      totalGradient += math.abs(current - down)
      count += 2
    }
    
    val averageGradient = totalGradient / count
    // Average gradient should be reasonable for alpine terrain
    averageGradient should be > 0.0  // Not flat
    averageGradient should be < 400.0 // Alpine terrain can have steep gradients
  }
}