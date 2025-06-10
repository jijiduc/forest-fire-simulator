package models

sealed trait CellState
case object Empty extends CellState
case object Tree extends CellState
case object Burning extends CellState
case object Burnt extends CellState

sealed trait VegetationType {
  def fuelLoad: Double
  def ignitionThreshold: Double
  def burnRate: Double
}

object VegetationType {
  case object DenseForest extends VegetationType {
    val fuelLoad = 0.9
    val ignitionThreshold = 0.6
    val burnRate = 0.8
  }
  
  case object SparseForest extends VegetationType {
    val fuelLoad = 0.7
    val ignitionThreshold = 0.5
    val burnRate = 0.7
  }
  
  case object Grassland extends VegetationType {
    val fuelLoad = 0.4
    val ignitionThreshold = 0.3
    val burnRate = 0.9
  }
  
  case object Shrubland extends VegetationType {
    val fuelLoad = 0.6
    val ignitionThreshold = 0.4
    val burnRate = 0.85
  }
  
  case object Barren extends VegetationType {
    val fuelLoad = 0.0
    val ignitionThreshold = 1.0
    val burnRate = 0.0
  }
  
  case object Water extends VegetationType {
    val fuelLoad = 0.0
    val ignitionThreshold = 1.0
    val burnRate = 0.0
  }
  
  case object Urban extends VegetationType {
    val fuelLoad = 0.1
    val ignitionThreshold = 0.9
    val burnRate = 0.2
  }
  
  // Legacy aliases for compatibility
  val NoVegetation = Barren
  val AlpineMeadow = Grassland
  val ConiferousForest = SparseForest
  val DeciduousForest = DenseForest
  val MixedForest = DenseForest
  
  // Determine vegetation type based on elevation
  def fromElevation(elevation: Double): VegetationType = {
    if (elevation < 1000) DenseForest
    else if (elevation < 1500) SparseForest
    else if (elevation < 2000) Shrubland
    else if (elevation < 2500) Grassland
    else Barren
  }
}