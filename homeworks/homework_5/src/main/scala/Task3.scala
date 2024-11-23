import cats._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

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
      def empty: WordsCount = WordsCount(Seq.empty)

      def combine(a: WordsCount, b: WordsCount): WordsCount = {
        val combinedCounts = (a.count ++ b.count)
          .groupBy(_.word)
          .map { case (word, counts) => Count(word, counts.map(_.count).sum)}

        WordsCount(combinedCounts.toVector)
      }
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val wordCountFunc: String => WordsCount = line => {
      val counts = line.split(" ").filter(_.nonEmpty)
        .groupBy(identity).view.map { case (word, occurrences) => Count(word, occurrences.length)}
      WordsCount(counts.toVector)
    }

    val futureResult = mapReduce(lines)(wordCountFunc)
    Await.result(futureResult, 10.seconds)
  }
}
