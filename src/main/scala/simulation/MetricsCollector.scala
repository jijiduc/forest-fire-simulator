package simulation

import models._
import scala.collection.mutable

/**
 * Collects and calculates metrics from simulation state
 */
object MetricsCollector {
  
  /**
   * Collect all metrics from current state
   */
  def collectMetrics(state: SimulationState): SimulationMetrics = {
    val grid = state.grid
    val cells = grid.cells.flatten
    
    // Count different cell states
    val activeFires = cells.count(_.state == Burning)
    val burntCells = cells.count(_.state == Burnt)
    val treeCells = cells.count(_.state == Tree)
    val totalCells = cells.length
    
    // Calculate densities
    val treeDensity = treeCells.toDouble / totalCells
    
    // Calculate average moisture
    val avgMoisture = cells.map(_.moisture).sum / totalCells
    
    // Find largest fire cluster
    val largestCluster = findLargestFireCluster(grid)
    
    // Calculate average fire intensity (temperature of burning cells)
    val avgIntensity = if (activeFires > 0) {
      cells.filter(_.state == Burning).map(_.temperature).sum / activeFires
    } else {
      0.0
    }
    
    // Calculate percolation indicator
    val percolationIndicator = calculatePercolationIndicator(grid, largestCluster)
    
    SimulationMetrics(
      activeFires = activeFires,
      totalBurntArea = burntCells,
      largestFireClusterSize = largestCluster,
      averageFireIntensity = avgIntensity,
      percolationIndicator = percolationIndicator,
      treeDensity = treeDensity,
      averageMoisture = avgMoisture
    )
  }
  
  /**
   * Find the size of the largest connected fire cluster
   */
  private def findLargestFireCluster(grid: Grid): Int = {
    val visited = mutable.Set[(Int, Int)]()
    var largestSize = 0
    
    for {
      y <- 0 until grid.height
      x <- 0 until grid.width
      if !visited.contains((x, y))
      if grid(x, y).state == Burning
    } {
      val clusterSize = exploreCluster(grid, x, y, visited)
      largestSize = math.max(largestSize, clusterSize)
    }
    
    largestSize
  }
  
  /**
   * Explore a fire cluster using DFS
   */
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
      
      if (!visited.contains((x, y)) && grid.get(x, y).exists(_.state == Burning)) {
        visited += ((x, y))
        size += 1
        
        // Add neighbors to stack
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
  
  /**
   * Calculate percolation indicator (0-1)
   * 1 indicates system-spanning fire
   */
  private def calculatePercolationIndicator(grid: Grid, largestCluster: Int): Double = {
    // Simple indicator: ratio of largest cluster to total grid size
    val totalSize = grid.width * grid.height
    val clusterRatio = largestCluster.toDouble / totalSize
    
    // Check if fire spans from one edge to another
    val spansSystem = checkSystemSpanning(grid)
    
    if (spansSystem) {
      1.0
    } else {
      // Use sigmoid to smooth the transition
      1.0 / (1.0 + math.exp(-10.0 * (clusterRatio - 0.1)))
    }
  }
  
  /**
   * Check if fire spans from one edge to another
   */
  private def checkSystemSpanning(grid: Grid): Boolean = {
    // Check if any burning path connects left to right edges
    val leftEdgeFires = (0 until grid.height).filter(y => grid(0, y).state == Burning)
    val rightEdgeFires = (0 until grid.height).filter(y => grid(grid.width - 1, y).state == Burning)
    
    if (leftEdgeFires.isEmpty || rightEdgeFires.isEmpty) {
      return false
    }
    
    // Use BFS to check connectivity
    val visited = mutable.Set[(Int, Int)]()
    val queue = mutable.Queue[(Int, Int)]()
    
    // Start from all left edge fires
    leftEdgeFires.foreach(y => queue.enqueue((0, y)))
    
    while (queue.nonEmpty) {
      val (x, y) = queue.dequeue()
      
      if (!visited.contains((x, y)) && grid.get(x, y).exists(_.state == Burning)) {
        visited += ((x, y))
        
        // Check if we reached the right edge
        if (x == grid.width - 1) {
          return true
        }
        
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
   * Calculate fire size distribution for phase analysis
   */
  def calculateFireSizeDistribution(grid: Grid): Map[Int, Int] = {
    val visited = mutable.Set[(Int, Int)]()
    val clusterSizes = mutable.ListBuffer[Int]()
    
    for {
      y <- 0 until grid.height
      x <- 0 until grid.width
      if !visited.contains((x, y))
      if grid(x, y).state == Burning || grid(x, y).state == Burnt
    } {
      val clusterSize = exploreCluster(grid, x, y, visited)
      clusterSizes += clusterSize
    }
    
    // Create histogram
    clusterSizes.groupBy(identity).mapValues(_.size).toMap
  }
}