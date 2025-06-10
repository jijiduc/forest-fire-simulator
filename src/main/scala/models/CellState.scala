package models

sealed trait CellState
case object Empty extends CellState
case object Tree extends CellState
case object Burning extends CellState
case object Burnt extends CellState

sealed trait VegetationType

object VegetationType {
  case object DenseForest extends VegetationType
  case object SparseForest extends VegetationType
  case object Grassland extends VegetationType
  case object Shrubland extends VegetationType
  case object Barren extends VegetationType
  case object Water extends VegetationType
  case object Urban extends VegetationType
  
  // Legacy aliases for compatibility
  val NoVegetation = Barren
  val AlpineMeadow = Grassland
  val ConiferousForest = SparseForest
  val DeciduousForest = DenseForest
  val MixedForest = DenseForest
}