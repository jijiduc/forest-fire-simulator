package models

case class Grid(cells: Vector[Vector[Cell]], width: Int, height: Int) {
  require(cells.length == height && cells.forall(_.length == width))
  
  def apply(x: Int, y: Int): Cell = cells(y)(x)
  
  def get(x: Int, y: Int): Option[Cell] = 
    if (x >= 0 && x < width && y >= 0 && y < height) Some(cells(y)(x))
    else None
  
  def updated(x: Int, y: Int, cell: Cell): Grid = {
    val newRow = cells(y).updated(x, cell)
    val newCells = cells.updated(y, newRow)
    copy(cells = newCells)
  }
  
  def map(f: Cell => Cell): Grid = {
    val newCells = cells.map(_.map(f))
    copy(cells = newCells)
  }
  
  def neighbors(x: Int, y: Int): List[Cell] = {
    val positions = for {
      dx <- -1 to 1
      dy <- -1 to 1
      if !(dx == 0 && dy == 0)
    } yield (x + dx, y + dy)
    
    positions.flatMap { case (nx, ny) => get(nx, ny) }.toList
  }
  
  def cellsWithPosition: Seq[(Int, Int, Cell)] = {
    for {
      y <- 0 until height
      x <- 0 until width
    } yield (x, y, cells(y)(x))
  }
}