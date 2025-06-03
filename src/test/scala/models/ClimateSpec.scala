package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClimateSpec extends AnyFlatSpec with Matchers {
  
  "Climate" should "correctly calculate temperature at elevation" in {
    val climate = Climate(
      season = Summer,
      wind = Wind(0, 5),
      humidity = 0.5,
      precipitation = 0.0
    )
    
    // Base temperature at sea level
    climate.temperatureAtElevation(0) shouldBe Summer.baseTemperature
    
    // Temperature should decrease with elevation (6.5°C per 1000m)
    val elevationTemp = climate.temperatureAtElevation(1000)
    elevationTemp shouldBe (Summer.baseTemperature - 6.5) +- 0.01
    
    climate.temperatureAtElevation(2000) shouldBe (Summer.baseTemperature - 13.0) +- 0.01
  }
  
  it should "correctly calculate oxygen factor at elevation" in {
    val climate = Climate(Summer, Wind(0, 5), 0.5, 0.0)
    
    climate.oxygenFactorAtElevation(0) shouldBe 1.0
    climate.oxygenFactorAtElevation(5000) shouldBe 0.5 +- 0.01
    climate.oxygenFactorAtElevation(10000) shouldBe 0.0
  }
  
  "Wind" should "correctly calculate directional components" in {
    // Wind blowing east (0 radians)
    val eastWind = Wind(0, 10)
    eastWind.dx shouldBe 10.0 +- 0.01
    eastWind.dy shouldBe 0.0 +- 0.01
    
    // Wind blowing north (π/2 radians)
    val northWind = Wind(math.Pi / 2, 10)
    northWind.dx shouldBe 0.0 +- 0.01
    northWind.dy shouldBe 10.0 +- 0.01
    
    // Wind blowing northeast (π/4 radians)
    val neWind = Wind(math.Pi / 4, 10)
    neWind.dx shouldBe (10 * math.sqrt(2) / 2) +- 0.01
    neWind.dy shouldBe (10 * math.sqrt(2) / 2) +- 0.01
  }
}