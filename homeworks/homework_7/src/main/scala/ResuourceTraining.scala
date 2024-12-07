package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}


/**
 * Необходимо реализовать функции readData и writeData, записывающие и читающие данные в/из файла соответственно.
 * В реализации следует применять безопасное использование ресурсов ZIO.acquireReleaseWith
 */


object ResuourceTraining extends ZIOAppDefault {

  def readData(filePath: String): IO[Throwable, String] = {
    ZIO.acquireReleaseWith(ZIO.attempt(new BufferedReader(new FileReader(filePath))))(
      file => ZIO.attempt(file.close()).orDie)(reader => ZIO.attempt(reader.readLine()))
  }

  def writeData(filePath: String, data: String): ZIO[Any, Throwable, Unit] = {
    ZIO.acquireReleaseWith(ZIO.attempt(new BufferedWriter(new FileWriter(filePath))))(
      file => ZIO.attempt(file.close()).orDie)(writer =>
      ZIO.attempt {
        writer.write(data)
        writer.flush()
      })
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
