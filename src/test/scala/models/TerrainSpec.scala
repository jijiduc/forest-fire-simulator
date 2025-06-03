package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TerrainSpec extends AnyFlatSpec with Matchers {
  
  def createTestTerrain(): Terrain = {
    val elevationMap = Vector(
      Vector(1000.0, 1100.0, 1200.0),
      Vector(1100.0, 1500.0, 1600.0),
      Vector(1200.0, 1600.0, 2500.0)
    )
    Terrain(elevationMap, 3, 3)
  }
  
  "Terrain" should "correctly retrieve elevation at coordinates" in {
    val terrain = createTestTerrain()
    
    terrain.elevationAt(0, 0) shouldBe 1000.0
    terrain.elevationAt(1, 1) shouldBe 1500.0
    terrain.elevationAt(2, 2) shouldBe 2500.0
  }
  
  it should "correctly calculate slope" in {
    val terrain = createTestTerrain()
    
    // Flat area should have zero slope
    val flatTerrain = Terrain(Vector.fill(3, 3)(1000.0), 3, 3)
    flatTerrain.slopeAt(1, 1) shouldBe 0.0
    
    // Sloped area should have positive slope
    terrain.slopeAt(1, 1) should be > 0.0
  }
  
  it should "correctly assign vegetation types based on elevation" in {
    val terrain = createTestTerrain()
    
    // Create test terrain with varied elevations
    val elevationMap = Vector(
      Vector(700.0, 1200.0, 1800.0),
      Vector(2100.0, 2300.0, 2600.0),
      Vector(1000.0, 1600.0, 3000.0)
    )
    val variedTerrain = Terrain(elevationMap, 3, 3)
    
    variedTerrain.vegetationTypeAt(0, 0) shouldBe MixedForest // < 800m
    variedTerrain.vegetationTypeAt(1, 0) shouldBe ConiferousForest // 800-2200m
    variedTerrain.vegetationTypeAt(1, 1) shouldBe AlpineMeadow // 2200-2400m
    variedTerrain.vegetationTypeAt(2, 1) shouldBe NoVegetation // > 2400m
  }
  
  it should "correctly calculate aspect" in {
    val terrain = createTestTerrain()
    
    // Aspect should be in radians (-π to π)
    val aspect = terrain.aspectAt(1, 1)
    aspect should be >= -math.Pi
    aspect should be <= math.Pi
  }
}