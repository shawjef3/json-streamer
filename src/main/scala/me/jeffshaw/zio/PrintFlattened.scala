package me.jeffshaw.zio

import com.fasterxml.jackson.core.JsonFactory
import java.io.{IOException, InputStream}
import java.nio.file.{Files, Paths}
import zio.stream.ZStream
import zio.{Console, NonEmptyChunk, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object PrintFlattened extends ZIOAppDefault {

  val factory = new JsonFactory()

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    for {
      args <- ZIOAppArgs.getArgs
      ins: ZStream[Scope, IOException, InputStream] =
        NonEmptyChunk.fromChunk(args)
          .fold[ZStream[Scope, IOException, InputStream]](
            ZStream.fromZIO(ZIO.succeed(System.in))
          )(nonEmptyChunk => ZStream.fromChunk(nonEmptyChunk).mapZIO(arg => ZIO.fromAutoCloseable(ZIO.attemptBlockingIO(Files.newInputStream(Paths.get(arg))))))
      _ <-
        ins.foreach { in =>
          ValuedJsonToken.toJValues(Decider.Stream, ValuedJsonToken.stream(factory.createParser(in)))
            .foreach { case (path, value) =>
                Console.printLine(s"$path = $value")
            }
        }
    } yield ()
}
