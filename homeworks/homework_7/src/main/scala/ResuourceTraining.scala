package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}


/**
 * Необходимо реализовать функции readData и writeData, записывающие и читающие данные в/из файла соответственно.
 * В реализации следует применять безопасное использование ресурсов ZIO.acquireReleaseWith
 */


object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = {
    ZIO.acquireReleaseWith(
      ZIO.attempt {
        new BufferedReader(
          new FileReader(
            filePath
          )
        )
      }.orDie
    )(
      read => ZIO.attempt {
        read.close()
      }.orDie
    )(
      read => ZIO.attempt {
        val data = Stream.continually(
          read.readLine()
        )
          .takeWhile(_ != null)
          .mkString("\n")
        read.close()
        data
      }.orDie
    )
  }

  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    ZIO.acquireReleaseWith(
      ZIO.attempt {
        new BufferedWriter(
          new FileWriter(
            filePath
          )
        )
      }.orDie
    )(
      write => ZIO.attempt {
        write.close()
      }.orDie
    )(
      write => ZIO.attempt {
        write.write(data)
        write.close()
      }.orDie
    )
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
