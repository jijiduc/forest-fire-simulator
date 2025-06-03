package models

case class Position(x: Int, y: Int)

case class Cell(
  position: Position,
  state: CellState,
  elevation: Double,
  vegetationType: VegetationType,
  moisture: Double,
  temperature: Double
) {
  def ignite: Cell = state match {
    case Tree => copy(state = Burning)
    case _ => this
  }
  
  def burnOut: Cell = state match {
    case Burning => copy(state = Burnt)
    case _ => this
  }
  
  def isFlammable: Boolean = state == Tree
  def isBurning: Boolean = state == Burning
}