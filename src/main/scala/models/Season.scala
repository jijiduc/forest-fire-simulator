package models

sealed trait Season {
  def baseTemperature: Double
  def baseHumidity: Double
  def precipitationProbability: Double
  def snowLineElevation: Double
}

case object Spring extends Season {
  val baseTemperature = 15.0
  val baseHumidity = 0.65
  val precipitationProbability = 0.3
  val snowLineElevation = 2000.0
}

case object Summer extends Season {
  val baseTemperature = 25.0
  val baseHumidity = 0.40
  val precipitationProbability = 0.15
  val snowLineElevation = 3000.0
}

case object Fall extends Season {
  val baseTemperature = 10.0
  val baseHumidity = 0.55
  val precipitationProbability = 0.25
  val snowLineElevation = 2200.0
}

case object Winter extends Season {
  val baseTemperature = -5.0
  val baseHumidity = 0.70
  val precipitationProbability = 0.4
  val snowLineElevation = 800.0
}

object Season {
  val Spring = models.Spring
  val Summer = models.Summer
  val Fall = models.Fall
  val Winter = models.Winter
}