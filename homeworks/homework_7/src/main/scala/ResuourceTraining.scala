package ru.dru

import zio.{IO, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}


/**
 * Необходимо реализовать функции readData и writeData, записывающие и читающие данные в/из файла соответственно.
 * В реализации следует применять безопасное использование ресурсов ZIO.acquireReleaseWith
 */


object ResuourceTraining extends ZIOAppDefault {

    def readData(filePath: String): IO[Throwable, String] = {
        val acquire = ZIO.attempt(new BufferedReader(new FileReader(filePath)))

        def release(reader: BufferedReader) = ZIO.attempt(reader.close()).orDie

        def read(reader: BufferedReader) = ZIO.attempt {
            val sb = new StringBuilder()
            var line: String = null
            while ( {
                line = reader.readLine();
                line != null
            }) {
                sb.append(line).append(System.lineSeparator())
            }
            sb.toString().stripTrailing()
        }

        ZIO.acquireReleaseWith(acquire)(release)(read)
    }

    def writeData(filePath: String, data: String): ZIO[Any, Nothing, Unit] = {
        val acquire = ZIO.attempt(new BufferedWriter(new FileWriter(filePath)))

        def release(writer: BufferedWriter) = ZIO.attempt(writer.close()).orDie

        def write(writer: BufferedWriter) = ZIO.attempt {
            writer.write(data)
            writer.flush()
        }.orDie

        ZIO.acquireReleaseWith(acquire)(release)(write).orElse(ZIO.unit)
    }

    override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.succeed("Done")
}
