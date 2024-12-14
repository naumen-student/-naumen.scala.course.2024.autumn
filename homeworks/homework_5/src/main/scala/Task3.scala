import cats._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, DurationInt}

/*
  Задание №3
  Всё просто, нужно посчитать количество строк.
  Реализуйте функцию countWords, которая принимает список строк.
  Обязательно использовать функцию mapReduce.
 */
object Task3 extends App {
  def mapReduce[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.foldMap(func)))
      .map(_.combineAll)
  }

  case class Count(word: String, count: Int)
  case class WordsCount(count: Seq[Count])
  object WordsCount {
    implicit val monoid: Monoid[WordsCount] = new Monoid[WordsCount] {
      override def empty: WordsCount = new WordsCount(count = Seq[Count]())

      override def combine(x: WordsCount, y: WordsCount): WordsCount = {
        val combinedCounts = (x.count ++ y.count)
          .groupBy(_.word)
          .map { case (word, counts) =>
            Count(word, counts.map(_.count).sum)
          }
          .toSeq

        WordsCount(combinedCounts)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val ft = mapReduce[String, WordsCount](lines) { line =>
      val wordCounts = line.split(' ').toVector
        .groupBy(identity)
        .map { case (word, occurrences) =>
          Count(word, occurrences.length)
        }
        .toSeq

      WordsCount(wordCounts)
    }

    Await.result(ft, Duration.Inf)
  }

}
