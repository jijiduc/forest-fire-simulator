package models

sealed trait CellState
case object Empty extends CellState
case object Tree extends CellState
case object Burning extends CellState
case object Burnt extends CellState

sealed trait VegetationType
case object NoVegetation extends VegetationType
case object AlpineMeadow extends VegetationType
case object ConiferousForest extends VegetationType
case object DeciduousForest extends VegetationType
case object MixedForest extends VegetationType