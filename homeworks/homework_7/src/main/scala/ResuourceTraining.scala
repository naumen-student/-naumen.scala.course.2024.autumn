package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}
import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}

object ResuourceTraining extends ZIOAppDefault {

  /**
   * Читает данные из файла по заданному пути.
   * Использует безопасное управление ресурсами с помощью ZIO.acquireReleaseWith.
   *
   * @param filePath путь к файлу для чтения
   * @return Содержимое файла как строка
   */
  def readData(filePath: String): IO[Throwable, String] = {
    ZIO.acquireReleaseWith(
        ZIO.attempt(new BufferedReader(new FileReader(filePath))))(
      reader => ZIO.attempt(reader.close()).orDie)(
      reader => ZIO.attempt(reader.readLine()))
  }

  /**
   * Записывает данные в файл по заданному пути.
   * Использует безопасное управление ресурсами с помощью ZIO.acquireReleaseWith.
   *
   * @param filePath путь к файлу для записи
   * @param data     данные для записи
   * @return ZIO эффект, завершающийся Unit
   */
  def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
    ZIO.acquireReleaseWith(
        ZIO.attempt(new BufferedWriter(new FileWriter(filePath))))(
        writer => ZIO.attempt(writer.close()).orDie)(
      writer => ZIO.attempt{
        writer.write(data)
        writer.flush()
      }).orDie
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
