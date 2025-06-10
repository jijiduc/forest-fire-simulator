package models

case class Terrain(elevationMap: Vector[Vector[Double]], width: Int, height: Int) {
  require(elevationMap.length == height && elevationMap.forall(_.length == width))
  
  def elevationAt(x: Int, y: Int): Double = elevationMap(y)(x)
  
  def slopeAt(x: Int, y: Int): Double = {
    val current = elevationAt(x, y)
    val neighbors = for {
      dx <- -1 to 1
      dy <- -1 to 1
      if !(dx == 0 && dy == 0)
      if x + dx >= 0 && x + dx < width
      if y + dy >= 0 && y + dy < height
    } yield {
      val neighbor = elevationAt(x + dx, y + dy)
      math.abs(neighbor - current) / math.sqrt(dx * dx + dy * dy)
    }
    
    if (neighbors.isEmpty) 0.0 else neighbors.max
  }
  
  def aspectAt(x: Int, y: Int): Double = {
    val dx = if (x > 0 && x < width - 1) {
      elevationAt(x + 1, y) - elevationAt(x - 1, y)
    } else 0.0
    
    val dy = if (y > 0 && y < height - 1) {
      elevationAt(x, y + 1) - elevationAt(x, y - 1)
    } else 0.0
    
    math.atan2(dy, dx)
  }
  
  def vegetationTypeAt(x: Int, y: Int): VegetationType = {
    val elevation = elevationAt(x, y)
    elevation match {
      case e if e < 800 => VegetationType.DenseForest
      case e if e < 1500 => VegetationType.SparseForest
      case e if e < 2200 => VegetationType.SparseForest
      case e if e < 2400 => VegetationType.Grassland
      case _ => VegetationType.Barren
    }
  }
}