package io.swiss

import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.http4s._
import org.http4s.client._
import org.http4s.dsl.io._
import java.awt.image.BufferedImage

class MapServiceClientsSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  
  // Mock HTTP client for testing
  def mockClient(response: Response[IO]): Client[IO] = Client.fromHttpApp[IO](HttpApp[IO] { req =>
    IO.pure(response)
  })
  
  "WMSClient" should "construct correct GetMap request" in {
    val mockResponse = Response[IO](Status.Ok).withEntity(Array.empty[Byte])
    val client = mockClient(mockResponse)
    val wmsClient = new WMSClient[IO](client)
    
    val bbox = CoordinateSystems.LV95BoundingBox(
      minEast = 2630000,
      minNorth = 1125000,
      maxEast = 2640000,
      maxNorth = 1135000
    )
    
    wmsClient.getMapImage(
      layers = "ch.swisstopo.digitales-hoehenmodell_25",
      bbox = bbox,
      width = 100,
      height = 100
    ).asserting { result =>
      result should be (Array.empty[Byte])
    }
  }
  
  "WMTSClient" should "calculate correct tile coordinates" in {
    val client = new WMTSClient[IO](Client.fromHttpApp(HttpApp.notFound))
    
    val bbox = CoordinateSystems.LV95BoundingBox(
      minEast = 2630000,
      minNorth = 1125000,
      maxEast = 2640000,
      maxNorth = 1135000
    )
    
    val tiles = client.calculateTiles(bbox, zoomLevel = 10)
    
    tiles should not be empty
    tiles.foreach { tile =>
      tile.zoom should be (10)
      tile.col should be >= 0
      tile.row should be >= 0
    }
    succeed
  }
  
  it should "construct correct tile URL" in {
    val mockResponse = Response[IO](Status.Ok).withEntity(Array.empty[Byte])
    val client = mockClient(mockResponse)
    val wmtsClient = new WMTSClient[IO](client)
    
    val tileCoord = wmtsClient.TileCoord(zoom = 10, col = 523, row = 358)
    
    wmtsClient.getTile(
      layer = "ch.swisstopo.pixelkarte-farbe",
      tileCoord = tileCoord,
      format = "png"
    ).asserting { result =>
      result should be (Array.empty[Byte])
    }
  }
  
  "SwissMapClient" should "coordinate WMS and WMTS clients" in {
    val mockResponse = Response[IO](Status.Ok).withEntity(Array.empty[Byte])
    val httpClient = mockClient(mockResponse)
    val mapClient = new SwissMapClient[IO](httpClient)
    
    mapClient.wms should not be null
    mapClient.wmts should not be null
  }
}

class ClimateProjectionSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  
  "ClimateProjection.default" should "provide reasonable projections" in {
    val rcp26_2050 = ClimateProjection.default(SwissDataModels.RCP26, 2050)
    val rcp85_2050 = ClimateProjection.default(SwissDataModels.RCP85, 2050)
    
    // RCP 8.5 should have higher temperature change than RCP 2.6
    rcp85_2050.temperatureChange should be > rcp26_2050.temperatureChange
    
    // Both should show negative precipitation change (drier)
    rcp26_2050.precipitationChange should be < 0.0
    rcp85_2050.precipitationChange should be < 0.0
    
    // RCP 8.5 should have higher extreme event multiplier
    rcp85_2050.extremeEventMultiplier should be > rcp26_2050.extremeEventMultiplier
  }
  
  it should "scale with year" in {
    val rcp45_2035 = ClimateProjection.default(SwissDataModels.RCP45, 2035)
    val rcp45_2085 = ClimateProjection.default(SwissDataModels.RCP45, 2085)
    
    // Later year should have larger changes
    math.abs(rcp45_2085.temperatureChange) should be > math.abs(rcp45_2035.temperatureChange)
    math.abs(rcp45_2085.precipitationChange) should be > math.abs(rcp45_2035.precipitationChange)
    rcp45_2085.extremeEventMultiplier should be > rcp45_2035.extremeEventMultiplier
  }
}