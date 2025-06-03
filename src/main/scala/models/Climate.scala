package models

case class Wind(direction: Double, speed: Double) {
  def dx: Double = math.cos(direction) * speed
  def dy: Double = math.sin(direction) * speed
}

case class Climate(
  season: Season,
  wind: Wind,
  humidity: Double,
  precipitation: Double
) {
  def temperatureAtElevation(elevation: Double): Double = {
    val lapseRate = 6.5 / 1000.0
    season.baseTemperature - (elevation * lapseRate)
  }
  
  def oxygenFactorAtElevation(elevation: Double): Double = {
    1.0 - (elevation / 10000.0)
  }
}