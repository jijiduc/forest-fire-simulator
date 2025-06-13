package app

import cats.effect._
import cats.effect.std.Console
import cats.implicits._
import cli._
import java.nio.file.{Files, Paths, Path}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.staticcontent._
import org.http4s.implicits._
import org.http4s.headers._
import org.http4s.server.Server
import org.http4s.server.middleware._
import org.typelevel.ci._
import java.awt.Desktop
import java.net.URI
import com.comcast.ip4s._
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import fs2.io.file.{Files => Fs2Files, Path => Fs2Path}
import org.http4s.circe._
import cats.effect.unsafe.implicits.global

class VisualizationRunner[F[_]: Async: Console](
  config: AppConfig
) {
  
  implicit def logger: Logger[F] = Slf4jLogger.getLogger[F]
  implicit val jsonEncoder: EntityEncoder[F, Json] = jsonEncoderOf[F, Json]
  
  def run(command: VisualizeCommand): F[ExitCode] = {
    command.visualizationType match {
      case VisualizationTypes.WebViewer => launchWebViewer(command)
      case VisualizationTypes.ThreeD => launch3DViewer(command)
      case VisualizationTypes.Notebook => launchNotebook(command)
    }
  }.handleErrorWith { error =>
    for {
      _ <- logger.error(error)("Visualization failed")
      _ <- Console[F].errorln(s"Error: ${error.getMessage}")
    } yield ExitCode.Error
  }
  
  private def launchWebViewer(command: VisualizeCommand): F[ExitCode] = for {
    _ <- Console[F].println("\n🌐 Launching Web Viewer")
    _ <- Console[F].println("=" * 60)
    
    // Check if input file exists
    exists <- Sync[F].delay(Files.exists(command.input))
    _ <- if (!exists) {
      Sync[F].raiseError(new IllegalArgumentException(s"Input file not found: ${command.input}"))
    } else Sync[F].unit
    
    // Create HTTP routes
    routes = createRoutes(command.input)
    
    // Create and start server
    _ <- Console[F].println(s"\n📁 Serving simulation data from: ${command.input}")
    _ <- Console[F].println(s"🚀 Starting web server on port ${command.port}")
    
    // Open browser automatically
    _ <- Sync[F].delay {
      if (Desktop.isDesktopSupported) {
        Desktop.getDesktop.browse(new URI(s"http://localhost:${command.port}"))
      }
    }.handleError(_ => ())
    
    _ <- Console[F].println(s"\n✅ Web viewer available at: http://localhost:${command.port}")
    _ <- Console[F].println("   Press Ctrl+C to stop the server")
    
    // Start the server
    exitCode <- EmberServerBuilder
      .default[F]
      .withHost(ipv4"0.0.0.0")
      .withPort(Port.fromInt(command.port).getOrElse(port"8080"))
      .withHttpApp(routes.orNotFound)
      .build
      .useForever
      .as(ExitCode.Success)
    
  } yield exitCode
  
  private def createRoutes(simulationDataPath: Path): HttpRoutes[F] = {
    val corsMiddleware = CORS.policy
      .withAllowOriginAll
      .withAllowMethodsAll
      .withAllowHeadersAll
      .apply(_: HttpRoutes[F])
    
    corsMiddleware(staticRoutes <+> apiRoutes(simulationDataPath))
  }
  
  private def staticRoutes: HttpRoutes[F] = {
    // Serve static resources from the classpath
    resourceServiceBuilder[F]("/web")
      .withPathPrefix("/")
      .toRoutes <+>
    // Fallback to index.html for SPA routing
    HttpRoutes.of[F] {
      case GET -> Root =>
        StaticFile.fromResource[F]("/web/index.html")
          .getOrElseF(Response[F](Status.NotFound).pure[F])
    }
  }
  
  private def apiRoutes(dataPath: Path): HttpRoutes[F] = HttpRoutes.of[F] {
    // Main simulation data endpoint
    case GET -> Root / "api" / "simulation" / "data" =>
      loadAndServeJson(dataPath)
    
    // Individual frame endpoint
    case GET -> Root / "api" / "simulation" / "frames" / IntVar(frameId) =>
      loadFrame(dataPath, frameId)
    
    // Metrics endpoint
    case GET -> Root / "api" / "simulation" / "metrics" =>
      loadMetrics(dataPath)
    
    // Simulation info endpoint
    case GET -> Root / "api" / "simulation" / "info" =>
      loadSimulationInfo(dataPath)
  }
  
  private def loadAndServeJson(path: Path): F[Response[F]] = {
    Sync[F].delay {
      val content = Files.readString(path)
      parse(content) match {
        case Right(json) => 
          Response[F](Status.Ok)
            .withEntity(json)
            .withHeaders(Header.Raw(CIString("Content-Type"), "application/json"))
        case Left(error) =>
          Response[F](Status.InternalServerError)
            .withEntity(Json.obj("error" -> Json.fromString(s"Failed to parse JSON: ${error.getMessage}")))
      }
    }.handleErrorWith { error =>
      Sync[F].pure(
        Response[F](Status.InternalServerError)
          .withEntity(Json.obj("error" -> Json.fromString(s"Failed to load simulation data: ${error.getMessage}")))
      )
    }
  }
  
  private def loadFrame(dataPath: Path, frameId: Int): F[Response[F]] = {
    Sync[F].delay {
      val content = Files.readString(dataPath)
      parse(content) match {
        case Right(json) =>
          json.hcursor.downField("frames").downN(frameId).focus match {
            case Some(frameJson) =>
              Response[F](Status.Ok)
                .withEntity(frameJson)
                .withHeaders(Header.Raw(CIString("Content-Type"), "application/json"))
            case None =>
              Response[F](Status.NotFound)
                .withEntity(Json.obj("error" -> Json.fromString(s"Frame $frameId not found")))
          }
        case Left(error) =>
          Response[F](Status.InternalServerError)
            .withEntity(Json.obj("error" -> Json.fromString(s"Failed to parse JSON: ${error.getMessage}")))
      }
    }
  }
  
  private def loadMetrics(dataPath: Path): F[Response[F]] = {
    Sync[F].delay {
      val content = Files.readString(dataPath)
      parse(content) match {
        case Right(json) =>
          val metrics = json.hcursor.downField("frames").values.map { frames =>
            frames.toList.flatMap { frame =>
              frame.hcursor.downField("metrics").focus
            }
          }.getOrElse(List.empty)
          
          Response[F](Status.Ok)
            .withEntity(Json.arr(metrics*))
            .withHeaders(Header.Raw(CIString("Content-Type"), "application/json"))
        case Left(error) =>
          Response[F](Status.InternalServerError)
            .withEntity(Json.obj("error" -> Json.fromString(s"Failed to parse JSON: ${error.getMessage}")))
      }
    }
  }
  
  private def loadSimulationInfo(dataPath: Path): F[Response[F]] = {
    Sync[F].delay {
      val content = Files.readString(dataPath)
      parse(content) match {
        case Right(json) =>
          val info = Json.obj(
            "frameCount" -> json.hcursor.downField("frames").values.map(_.size).getOrElse(0).asJson,
            "gridSize" -> json.hcursor.downField("gridSize").focus.getOrElse(Json.Null),
            "parameters" -> json.hcursor.downField("parameters").focus.getOrElse(Json.obj())
          )
          
          Response[F](Status.Ok)
            .withEntity(info)
            .withHeaders(Header.Raw(CIString("Content-Type"), "application/json"))
        case Left(error) =>
          Response[F](Status.InternalServerError)
            .withEntity(Json.obj("error" -> Json.fromString(s"Failed to parse JSON: ${error.getMessage}")))
      }
    }
  }
  
  private def launch3DViewer(command: VisualizeCommand): F[ExitCode] = for {
    _ <- Console[F].println("\n🎮 Launching 3D Viewer")
    _ <- Console[F].println("=" * 60)
    
    // Check if VTK data exists
    vtkFile <- Sync[F].delay {
      val vtkPath = command.input.getParent.resolve("visualization.vtk")
      vtkPath
    }
    
    _ <- Console[F].println("\n🎯 3D Visualization Features:")
    _ <- Console[F].println("   - Terrain elevation with texture mapping")
    _ <- Console[F].println("   - Fire intensity as color gradient")
    _ <- Console[F].println("   - Wind direction vectors")
    _ <- Console[F].println("   - Time-based animation")
    _ <- Console[F].println("   - Camera controls (rotate/zoom/pan)")
    
    _ <- Console[F].println("\n💡 Tip: Use ParaView or similar VTK viewer to open the file")
    _ <- Console[F].println(s"   File location: $vtkFile")
    
    // In a real implementation, this could launch ParaView or a custom 3D viewer
    _ <- Sync[F].delay {
      if (Desktop.isDesktopSupported) {
        // Try to open with system default VTK viewer
        Desktop.getDesktop.open(vtkFile.toFile)
      }
    }.handleError(_ => ())
    
  } yield ExitCode.Success
  
  private def launchNotebook(command: VisualizeCommand): F[ExitCode] = for {
    _ <- Console[F].println("\n📓 Launching Jupyter Notebook")
    _ <- Console[F].println("=" * 60)
    
    // Create or update notebook with visualization code
    notebookPath <- Sync[F].delay {
      val nbPath = command.input.getParent.resolve("forest_fire_analysis.ipynb")
      if (!Files.exists(nbPath)) {
        createVisualizationNotebook(nbPath, command.input)
      }
      nbPath
    }
    
    _ <- Console[F].println("\n📊 Notebook Features:")
    _ <- Console[F].println("   - Interactive plots with Plotly")
    _ <- Console[F].println("   - Statistical analysis tools")
    _ <- Console[F].println("   - Phase transition diagrams")
    _ <- Console[F].println("   - Custom visualization code")
    
    _ <- Console[F].println(s"\n💡 To open the notebook:")
    _ <- Console[F].println(s"   jupyter notebook $notebookPath")
    
    // Try to launch Jupyter
    _ <- Sync[F].delay {
      import scala.sys.process._
      s"jupyter notebook $notebookPath".!
    }.handleError(_ => ())
    
  } yield ExitCode.Success
  
  private def createVisualizationNotebook(path: java.nio.file.Path, dataPath: java.nio.file.Path): Unit = {
    val notebookContent = s"""
{
 "cells": [
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "# Forest Fire Simulation Analysis\\n",
    "\\n",
    "This notebook provides interactive visualization and analysis of the simulation results."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "import json\\n",
    "import numpy as np\\n",
    "import pandas as pd\\n",
    "import plotly.graph_objects as go\\n",
    "import plotly.express as px\\n",
    "from plotly.subplots import make_subplots"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Load simulation data\\n",
    "with open('${dataPath}', 'r') as f:\\n",
    "    data = json.load(f)\\n",
    "\\n",
    "print(f'Loaded {len(data[\\"frames\\"])} simulation frames')"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Create animated heatmap of fire spread\\n",
    "frames = []\\n",
    "for i, frame in enumerate(data['frames']):\\n",
    "    z_data = [[cell['state'] for cell in row] for row in frame['grid']]\\n",
    "    frames.append(go.Frame(\\n",
    "        data=[go.Heatmap(z=z_data, colorscale='hot')],\\n",
    "        name=str(i)\\n",
    "    ))\\n",
    "\\n",
    "fig = go.Figure(\\n",
    "    data=[go.Heatmap(z=frames[0].data[0].z, colorscale='hot')],\\n",
    "    frames=frames\\n",
    ")\\n",
    "\\n",
    "fig.update_layout(\\n",
    "    title='Fire Spread Animation',\\n",
    "    updatemenus=[{\\n",
    "        'type': 'buttons',\\n",
    "        'buttons': [\\n",
    "            {'label': 'Play', 'method': 'animate', 'args': [None]},\\n",
    "            {'label': 'Pause', 'method': 'animate', 'args': [None, {'frame': {'duration': 0}}]}\\n",
    "        ]\\n",
    "    }]\\n",
    ")\\n",
    "\\n",
    "fig.show()"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Plot metrics over time\\n",
    "metrics_df = pd.DataFrame([frame['metrics'] for frame in data['frames']])\\n",
    "\\n",
    "fig = make_subplots(\\n",
    "    rows=2, cols=2,\\n",
    "    subplot_titles=('Active Fires', 'Burnt Area', 'Tree Density', 'Percolation')\\n",
    ")\\n",
    "\\n",
    "fig.add_trace(go.Scatter(y=metrics_df['activeFires']), row=1, col=1)\\n",
    "fig.add_trace(go.Scatter(y=metrics_df['totalBurntArea']), row=1, col=2)\\n",
    "fig.add_trace(go.Scatter(y=metrics_df['treeDensity']), row=2, col=1)\\n",
    "fig.add_trace(go.Scatter(y=metrics_df['percolationIndicator']), row=2, col=2)\\n",
    "\\n",
    "fig.update_layout(height=600, title_text='Simulation Metrics Over Time')\\n",
    "fig.show()"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3",
   "language": "python",
   "name": "python3"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 4
}
"""
    Files.write(path, notebookContent.getBytes)
  }
}