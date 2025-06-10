package simulation

import models._

/**
 * Handles boundary conditions for the simulation grid
 */
object BoundaryHandler {
  
  /**
   * Get neighbors considering boundary conditions
   */
  def getNeighborsWithBoundary(
    grid: Grid, 
    x: Int, 
    y: Int, 
    boundary: BoundaryCondition
  ): List[Cell] = boundary match {
    case PeriodicBoundary => getPeriodicNeighbors(grid, x, y)
    case ReflectiveBoundary => getReflectiveNeighbors(grid, x, y)
    case AbsorbingBoundary => getAbsorbingNeighbors(grid, x, y)
    case FixedBoundary(conditions) => getFixedNeighbors(grid, x, y, conditions)
  }
  
  /**
   * Periodic boundary - grid wraps around like a torus
   */
  private def getPeriodicNeighbors(grid: Grid, x: Int, y: Int): List[Cell] = {
    val neighbors = for {
      dx <- -1 to 1
      dy <- -1 to 1
      if !(dx == 0 && dy == 0)
    } yield {
      val nx = (x + dx + grid.width) % grid.width
      val ny = (y + dy + grid.height) % grid.height
      grid(nx, ny)
    }
    neighbors.toList
  }
  
  /**
   * Reflective boundary - treat edges as mirrors
   */
  private def getReflectiveNeighbors(grid: Grid, x: Int, y: Int): List[Cell] = {
    val neighbors = for {
      dx <- -1 to 1
      dy <- -1 to 1
      if !(dx == 0 && dy == 0)
    } yield {
      val nx = math.min(math.max(0, x + dx), grid.width - 1)
      val ny = math.min(math.max(0, y + dy), grid.height - 1)
      grid(nx, ny)
    }
    neighbors.toList.distinct // Remove duplicates from reflection
  }
  
  /**
   * Absorbing boundary - cells outside grid don't exist
   */
  private def getAbsorbingNeighbors(grid: Grid, x: Int, y: Int): List[Cell] = {
    grid.neighbors(x, y)
  }
  
  /**
   * Fixed boundary - specific conditions at edges
   */
  private def getFixedNeighbors(
    grid: Grid, 
    x: Int, 
    y: Int, 
    conditions: Map[Position, CellState]
  ): List[Cell] = {
    val neighbors = for {
      dx <- -1 to 1
      dy <- -1 to 1
      if !(dx == 0 && dy == 0)
      nx = x + dx
      ny = y + dy
    } yield {
      if (nx >= 0 && nx < grid.width && ny >= 0 && ny < grid.height) {
        grid(nx, ny)
      } else {
        // Create virtual cell with fixed state
        val position = Position(nx, ny)
        val fixedState = conditions.getOrElse(position, Empty)
        Cell(
          position = position,
          state = fixedState,
          elevation = 0.0,
          vegetationType = VegetationType.Barren,
          moisture = 1.0,
          temperature = 20.0
        )
      }
    }
    neighbors.toList
  }
  
  /**
   * Check if a position is valid considering boundary conditions
   */
  def isValidPosition(
    x: Int, 
    y: Int, 
    width: Int, 
    height: Int, 
    boundary: BoundaryCondition
  ): Boolean = boundary match {
    case PeriodicBoundary => true // All positions are valid with wrapping
    case _ => x >= 0 && x < width && y >= 0 && y < height
  }
  
  /**
   * Adjust position based on boundary condition
   */
  def adjustPosition(
    x: Int, 
    y: Int, 
    width: Int, 
    height: Int, 
    boundary: BoundaryCondition
  ): Option[(Int, Int)] = boundary match {
    case PeriodicBoundary => 
      Some(((x + width) % width, (y + height) % height))
    case ReflectiveBoundary =>
      Some((math.min(math.max(0, x), width - 1), math.min(math.max(0, y), height - 1)))
    case _ =>
      if (x >= 0 && x < width && y >= 0 && y < height) Some((x, y)) else None
  }
}