package terrain

import models._
import scala.util.Random

object TerrainGenerator {
  
  def generateTerrain(width: Int, height: Int, seed: Long = System.currentTimeMillis): Terrain = {
    val random = new Random(seed)
    val elevationMap = generateElevationMap(width, height, random)
    Terrain(elevationMap, width, height)
  }
  
  private def generateElevationMap(width: Int, height: Int, random: Random): Vector[Vector[Double]] = {
    val scale = 0.03  // Increased scale for smoother terrain
    val octaves = 3   // Reduced octaves for less detail
    val persistence = 0.5
    val lacunarity = 2.0
    
    Vector.tabulate(height, width) { (y, x) =>
      var elevation = 0.0
      var amplitude = 1.0
      var frequency = 1.0
      
      for (_ <- 0 until octaves) {
        val nx = x * scale * frequency
        val ny = y * scale * frequency
        elevation += perlinNoise(nx, ny, random) * amplitude
        amplitude *= persistence
        frequency *= lacunarity
      }
      
      val normalized = (elevation + 1) / 2
      normalized * 2500 + 500  // Reduced elevation range for smoother gradients
    }
  }
  
  private def perlinNoise(x: Double, y: Double, random: Random): Double = {
    val X = x.toInt
    val Y = y.toInt
    val xf = x - X
    val yf = y - Y
    
    val topLeft = dotGridGradient(X, Y, xf, yf, random)
    val topRight = dotGridGradient(X + 1, Y, xf - 1, yf, random)
    val bottomLeft = dotGridGradient(X, Y + 1, xf, yf - 1, random)
    val bottomRight = dotGridGradient(X + 1, Y + 1, xf - 1, yf - 1, random)
    
    val xt = fade(xf)
    val yt = fade(yf)
    
    val top = lerp(topLeft, topRight, xt)
    val bottom = lerp(bottomLeft, bottomRight, xt)
    lerp(top, bottom, yt)
  }
  
  private def dotGridGradient(ix: Int, iy: Int, x: Double, y: Double, random: Random): Double = {
    val gradient = randomGradient(ix, iy, random)
    gradient._1 * x + gradient._2 * y
  }
  
  private def randomGradient(ix: Int, iy: Int, random: Random): (Double, Double) = {
    // Use the random instance to generate deterministic but seed-dependent gradients
    val hash = (ix * 73856093) ^ (iy * 19349663) ^ random.nextInt()
    val angle = (hash & 0xFFFF) * 2.0 * math.Pi / 0xFFFF
    (math.cos(angle), math.sin(angle))
  }
  
  private def fade(t: Double): Double = t * t * t * (t * (t * 6 - 15) + 10)
  
  private def lerp(a: Double, b: Double, t: Double): Double = a + t * (b - a)
}