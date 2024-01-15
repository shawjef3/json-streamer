package me.jeffshaw.json

import com.fasterxml.jackson.core.JsonFactory
import java.nio.file.{Files, Paths}
import zio.stream.ZStream
import zio.{Console, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object PrintFlattened extends ZIOAppDefault {

  val factory = new JsonFactory()

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    for {
      args <- ZIOAppArgs.getArgs
      result <-
        ZStream.fromChunk(args).foreach { arg =>
          ZIO.scoped {
            for {
              in <- ZIO.fromAutoCloseable(ZIO.attemptBlockingIO(Files.newInputStream(Paths.get(arg))))
              parser <- ZIO.fromAutoCloseable(ZIO.attemptBlockingIO(factory.createParser(in)))
              result <- JsonZio.jsons(Decider.stream, JsonZio.tokens(parser))
                .foreach { case (path, value) =>
                  Console.printLine(s"$path = $value")
                }
            } yield result
          }
        }
    } yield result
}
