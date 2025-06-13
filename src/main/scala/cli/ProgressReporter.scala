package cli

import cats._
import cats.effect._
import cats.effect.std.Console
import cats.implicits._
import scala.concurrent.duration._

trait ProgressReporter[F[_]] {
  def start(total: Long, message: String): F[Unit]
  def update(current: Long): F[Unit]
  def updateWithMessage(current: Long, message: String): F[Unit]
  def finish(message: String): F[Unit]
}

object ProgressReporter {
  
  def console[F[_]: Temporal: Console]: F[ProgressReporter[F]] = 
    Ref.of[F, ProgressState](ProgressState.initial).map { stateRef =>
      new ConsoleProgressReporter[F](stateRef)
    }
  
  def silent[F[_]: Applicative]: ProgressReporter[F] = new ProgressReporter[F] {
    def start(total: Long, message: String): F[Unit] = Applicative[F].unit
    def update(current: Long): F[Unit] = Applicative[F].unit
    def updateWithMessage(current: Long, message: String): F[Unit] = Applicative[F].unit
    def finish(message: String): F[Unit] = Applicative[F].unit
  }
}

case class ProgressState(
  startTime: Long,
  total: Long,
  current: Long,
  message: String
) {
  def progress: Double = if (total > 0) current.toDouble / total else 0.0
  def percentage: Int = (progress * 100).toInt
  
  def estimatedTimeRemaining(currentTime: Long): Option[Duration] = {
    if (current > 0 && progress < 1.0) {
      val elapsed = currentTime - startTime
      val rate = current.toDouble / elapsed
      val remaining = (total - current) / rate
      Some(remaining.millis)
    } else None
  }
}

object ProgressState {
  def initial: ProgressState = ProgressState(0, 0, 0, "")
}

class ConsoleProgressReporter[F[_]: Temporal: Console](
  stateRef: Ref[F, ProgressState]
) extends ProgressReporter[F] {
  
  private val barWidth = 50
  private val updateInterval = 100.millis
  
  def start(total: Long, message: String): F[Unit] = for {
    now <- Clock[F].realTime.map(_.toMillis)
    _ <- stateRef.set(ProgressState(now, total, 0, message))
    _ <- renderProgress
  } yield ()
  
  def update(current: Long): F[Unit] = for {
    _ <- stateRef.update(_.copy(current = current))
    _ <- renderProgress
  } yield ()
  
  def updateWithMessage(current: Long, message: String): F[Unit] = for {
    _ <- stateRef.update(_.copy(current = current, message = message))
    _ <- renderProgress
  } yield ()
  
  def finish(message: String): F[Unit] = for {
    state <- stateRef.get
    _ <- stateRef.update(_.copy(current = state.total, message = message))
    _ <- renderProgress
    _ <- Console[F].println("") // New line after progress bar
  } yield ()
  
  private def renderProgress: F[Unit] = for {
    state <- stateRef.get
    now <- Clock[F].realTime.map(_.toMillis)
    _ <- Console[F].print(s"\r${formatProgress(state, now)}")
  } yield ()
  
  private def formatProgress(state: ProgressState, currentTime: Long): String = {
    val filled = (state.progress * barWidth).toInt
    val empty = barWidth - filled
    val bar = "█" * filled + "░" * empty
    
    val eta = state.estimatedTimeRemaining(currentTime) match {
      case Some(duration) => formatDuration(duration)
      case None => "--:--"
    }
    
    f"${state.message}%s [${bar}%s] ${state.percentage}%3d%% (${state.current}%d/${state.total}%d) ETA: ${eta}%s"
  }
  
  private def formatDuration(duration: Duration): String = {
    val totalSeconds = duration.toSeconds
    val hours = totalSeconds / 3600
    val minutes = (totalSeconds % 3600) / 60
    val seconds = totalSeconds % 60
    
    if (hours > 0) f"$hours%d:$minutes%02d:$seconds%02d"
    else f"$minutes%02d:$seconds%02d"
  }
}