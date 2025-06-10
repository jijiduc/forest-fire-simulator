package analysis

import models._
import simulation._
import scala.collection.mutable

/**
 * Calculates order parameters that characterize system phases
 * These are the key observables for identifying phase transitions
 */
object OrderParameters {
  
  /**
   * Global burnt fraction (Φ) - primary order parameter
   * Fraction of burnable cells that have burnt
   */
  def burntFraction(state: SimulationState): Double = {
    val cells = state.grid.cells.flatten
    val burnableCells = cells.filter(c => 
      c.state != Empty && c.vegetationType != VegetationType.Water
    )
    
    if (burnableCells.isEmpty) 0.0
    else {
      val burntCount = burnableCells.count(_.state == Burnt)
      burntCount.toDouble / burnableCells.length
    }
  }
  
  /**
   * Active fire fraction - fraction of cells currently burning
   */
  def activeFraction(state: SimulationState): Double = {
    val cells = state.grid.cells.flatten
    val totalCells = cells.length
    val burningCount = cells.count(_.state == Burning)
    
    burningCount.toDouble / totalCells
  }
  
  /**
   * Largest cluster size ratio (S_max / N)
   * Size of largest connected fire cluster relative to system size
   */
  def largestClusterRatio(state: SimulationState): Double = {
    val largestCluster = findLargestCluster(state.grid)
    val totalCells = state.grid.width * state.grid.height
    
    largestCluster.toDouble / totalCells
  }
  
  /**
   * Percolation indicator - binary indicator of system-spanning fire
   * 1 if fire connects opposite edges, 0 otherwise
   */
  def percolationIndicator(state: SimulationState): Double = {
    if (checkPercolation(state.grid)) 1.0 else 0.0
  }
  
  /**
   * Smooth percolation indicator using MetricsCollector
   * Returns value in [0,1] with smooth transition
   */
  def smoothPercolationIndicator(state: SimulationState): Double = {
    state.metrics.percolationIndicator
  }
  
  /**
   * Burn velocity - rate of fire spread
   * Calculated from time series of states
   */
  def burnVelocity(states: List[SimulationState]): Double = {
    if (states.length < 2) return 0.0
    
    val burntAreas = states.map(s => burntFraction(s))
    val times = states.map(_.elapsedTime)
    
    // Calculate average rate of change
    val velocities = for {
      i <- 1 until states.length
      dt = times(i) - times(i-1)
      if dt > 0
    } yield {
      (burntAreas(i) - burntAreas(i-1)) / dt
    }
    
    if (velocities.isEmpty) 0.0
    else velocities.sum / velocities.length
  }
  
  /**
   * Cluster number density - number of distinct fire clusters
   */
  def clusterDensity(state: SimulationState): Double = {
    val clusters = findAllClusters(state.grid)
    val totalCells = state.grid.width * state.grid.height
    
    clusters.length.toDouble / totalCells
  }
  
  /**
   * Average cluster size
   */
  def averageClusterSize(state: SimulationState): Double = {
    val clusters = findAllClusters(state.grid)
    if (clusters.isEmpty) 0.0
    else clusters.map(_.size).sum.toDouble / clusters.length
  }
  
  /**
   * Fire front length - perimeter of all burning regions
   */
  def fireFrontLength(state: SimulationState): Double = {
    var perimeter = 0
    val grid = state.grid
    
    for {
      y <- 0 until grid.height
      x <- 0 until grid.width
      if grid(x, y).state == Burning
    } {
      // Count edges adjacent to non-burning cells
      val neighbors = grid.neighbors(x, y)
      val nonBurningNeighbors = neighbors.count(_.state != Burning)
      perimeter += nonBurningNeighbors
    }
    
    perimeter.toDouble
  }
  
  /**
   * Correlation length - characteristic length scale of fire clusters
   */
  def correlationLength(state: SimulationState): Double = {
    val clusters = findAllClusters(state.grid)
    if (clusters.isEmpty) return 0.0
    
    // Simplified: use sqrt of average cluster size
    math.sqrt(averageClusterSize(state))
  }
  
  /**
   * Susceptibility - variance of order parameter (indicates criticality)
   */
  def susceptibility(states: List[SimulationState]): Double = {
    if (states.length < 2) return 0.0
    
    val burntFractions = states.map(burntFraction)
    val mean = burntFractions.sum / burntFractions.length
    val variance = burntFractions.map(x => math.pow(x - mean, 2)).sum / burntFractions.length
    
    variance * states.head.grid.width * states.head.grid.height // Scale with system size
  }
  
  // Helper methods
  
  private def findLargestCluster(grid: Grid): Int = {
    val visited = mutable.Set[(Int, Int)]()
    var largestSize = 0
    
    for {
      y <- 0 until grid.height
      x <- 0 until grid.width
      if !visited.contains((x, y))
      if grid(x, y).state == Burning || grid(x, y).state == Burnt
    } {
      val clusterSize = exploreCluster(grid, x, y, visited)
      largestSize = math.max(largestSize, clusterSize)
    }
    
    largestSize
  }
  
  private def findAllClusters(grid: Grid): List[Set[(Int, Int)]] = {
    val visited = mutable.Set[(Int, Int)]()
    val clusters = mutable.ListBuffer[Set[(Int, Int)]]()
    
    for {
      y <- 0 until grid.height
      x <- 0 until grid.width
      if !visited.contains((x, y))
      if grid(x, y).state == Burning || grid(x, y).state == Burnt
    } {
      val cluster = mutable.Set[(Int, Int)]()
      exploreClusterSet(grid, x, y, visited, cluster)
      if (cluster.nonEmpty) clusters += cluster.toSet
    }
    
    clusters.toList
  }
  
  private def exploreCluster(
    grid: Grid,
    startX: Int,
    startY: Int,
    visited: mutable.Set[(Int, Int)]
  ): Int = {
    val stack = mutable.Stack[(Int, Int)]()
    stack.push((startX, startY))
    var size = 0
    
    while (stack.nonEmpty) {
      val (x, y) = stack.pop()
      
      if (!visited.contains((x, y)) && grid.get(x, y).exists(c => 
        c.state == Burning || c.state == Burnt)) {
        visited += ((x, y))
        size += 1
        
        // Add neighbors
        for {
          dx <- -1 to 1
          dy <- -1 to 1
          if !(dx == 0 && dy == 0)
          nx = x + dx
          ny = y + dy
          if nx >= 0 && nx < grid.width && ny >= 0 && ny < grid.height
        } {
          stack.push((nx, ny))
        }
      }
    }
    
    size
  }
  
  private def exploreClusterSet(
    grid: Grid,
    startX: Int,
    startY: Int,
    visited: mutable.Set[(Int, Int)],
    cluster: mutable.Set[(Int, Int)]
  ): Unit = {
    val stack = mutable.Stack[(Int, Int)]()
    stack.push((startX, startY))
    
    while (stack.nonEmpty) {
      val (x, y) = stack.pop()
      
      if (!visited.contains((x, y)) && grid.get(x, y).exists(c => 
        c.state == Burning || c.state == Burnt)) {
        visited += ((x, y))
        cluster += ((x, y))
        
        // Add neighbors
        for {
          dx <- -1 to 1
          dy <- -1 to 1
          if !(dx == 0 && dy == 0)
          nx = x + dx
          ny = y + dy
          if nx >= 0 && nx < grid.width && ny >= 0 && ny < grid.height
        } {
          stack.push((nx, ny))
        }
      }
    }
  }
  
  private def checkPercolation(grid: Grid): Boolean = {
    // Check if fire connects left to right edges
    val leftEdgeFires = (0 until grid.height).filter(y => 
      grid(0, y).state == Burning || grid(0, y).state == Burnt
    )
    
    if (leftEdgeFires.isEmpty) return false
    
    val visited = mutable.Set[(Int, Int)]()
    val queue = mutable.Queue[(Int, Int)]()
    
    // Start from all left edge fires
    leftEdgeFires.foreach(y => queue.enqueue((0, y)))
    
    while (queue.nonEmpty) {
      val (x, y) = queue.dequeue()
      
      if (!visited.contains((x, y)) && grid.get(x, y).exists(c =>
        c.state == Burning || c.state == Burnt)) {
        visited += ((x, y))
        
        // Check if we reached right edge
        if (x == grid.width - 1) return true
        
        // Add neighbors
        for {
          dx <- -1 to 1
          dy <- -1 to 1
          if !(dx == 0 && dy == 0)
          nx = x + dx
          ny = y + dy
          if nx >= 0 && nx < grid.width && ny >= 0 && ny < grid.height
        } {
          queue.enqueue((nx, ny))
        }
      }
    }
    
    false
  }
  
  /**
   * Calculate all order parameters at once for efficiency
   */
  def calculateAll(state: SimulationState): Map[String, Double] = Map(
    "burntFraction" -> burntFraction(state),
    "activeFraction" -> activeFraction(state),
    "largestClusterRatio" -> largestClusterRatio(state),
    "percolationIndicator" -> percolationIndicator(state),
    "smoothPercolation" -> smoothPercolationIndicator(state),
    "clusterDensity" -> clusterDensity(state),
    "averageClusterSize" -> averageClusterSize(state),
    "fireFrontLength" -> fireFrontLength(state),
    "correlationLength" -> correlationLength(state)
  )
}