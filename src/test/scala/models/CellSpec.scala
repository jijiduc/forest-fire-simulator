package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CellSpec extends AnyFlatSpec with Matchers {
  
  "A Cell" should "correctly ignite when it's a tree" in {
    val cell = Cell(
      position = Position(0, 0),
      state = Tree,
      elevation = 1000.0,
      vegetationType = ConiferousForest,
      moisture = 0.5,
      temperature = 20.0
    )
    
    val ignited = cell.ignite
    ignited.state shouldBe Burning
  }
  
  it should "not ignite when it's not a tree" in {
    val emptyCell = Cell(Position(0, 0), Empty, 1000.0, NoVegetation, 0.5, 20.0)
    val burningCell = Cell(Position(0, 0), Burning, 1000.0, ConiferousForest, 0.5, 20.0)
    val burntCell = Cell(Position(0, 0), Burnt, 1000.0, ConiferousForest, 0.5, 20.0)
    
    emptyCell.ignite.state shouldBe Empty
    burningCell.ignite.state shouldBe Burning
    burntCell.ignite.state shouldBe Burnt
  }
  
  it should "correctly burn out when burning" in {
    val burningCell = Cell(Position(0, 0), Burning, 1000.0, ConiferousForest, 0.5, 20.0)
    val burntOut = burningCell.burnOut
    burntOut.state shouldBe Burnt
  }
  
  it should "not burn out when not burning" in {
    val treeCell = Cell(Position(0, 0), Tree, 1000.0, ConiferousForest, 0.5, 20.0)
    treeCell.burnOut.state shouldBe Tree
  }
  
  it should "correctly identify flammable cells" in {
    val treeCell = Cell(Position(0, 0), Tree, 1000.0, ConiferousForest, 0.5, 20.0)
    val emptyCell = Cell(Position(0, 0), Empty, 1000.0, NoVegetation, 0.5, 20.0)
    
    treeCell.isFlammable shouldBe true
    emptyCell.isFlammable shouldBe false
  }
  
  it should "correctly identify burning cells" in {
    val burningCell = Cell(Position(0, 0), Burning, 1000.0, ConiferousForest, 0.5, 20.0)
    val treeCell = Cell(Position(0, 0), Tree, 1000.0, ConiferousForest, 0.5, 20.0)
    
    burningCell.isBurning shouldBe true
    treeCell.isBurning shouldBe false
  }
}