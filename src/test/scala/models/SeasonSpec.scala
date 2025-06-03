package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SeasonSpec extends AnyFlatSpec with Matchers {
  
  "Seasons" should "have appropriate temperature values" in {
    Summer.baseTemperature should be > Spring.baseTemperature
    Spring.baseTemperature should be > Fall.baseTemperature
    Fall.baseTemperature should be > Winter.baseTemperature
  }
  
  it should "have appropriate humidity values" in {
    // Summer should be drier
    Summer.baseHumidity should be < Spring.baseHumidity
    Summer.baseHumidity should be < Fall.baseHumidity
    Summer.baseHumidity should be < Winter.baseHumidity
  }
  
  it should "have appropriate precipitation probabilities" in {
    // Summer should have less precipitation
    Summer.precipitationProbability should be < Spring.precipitationProbability
    Summer.precipitationProbability should be < Winter.precipitationProbability
  }
  
  it should "have appropriate snow line elevations" in {
    // Snow line should be highest in summer, lowest in winter
    Summer.snowLineElevation should be > Spring.snowLineElevation
    Summer.snowLineElevation should be > Fall.snowLineElevation
    Summer.snowLineElevation should be > Winter.snowLineElevation
    
    Winter.snowLineElevation should be < 1000.0 // Snow in valleys during winter
    Summer.snowLineElevation should be > 2500.0 // Snow only on high peaks in summer
  }
}