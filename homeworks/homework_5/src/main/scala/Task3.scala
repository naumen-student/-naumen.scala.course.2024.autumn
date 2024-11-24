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
      def empty: WordsCount = new WordsCount(Seq.empty)

      def combine(wordsCount1: WordsCount, wordsCount2: WordsCount): WordsCount =
        new WordsCount(
          (wordsCount1.count ++ wordsCount2.count)
            .groupBy(_.word)
            .map(el => Count(el._1, el._2.map(_.count).sum))
            .toSeq
        )
    }
  }

  def countWords(lines: Vector[String]): WordsCount = {
    val wordCountFunc: String => WordsCount = line => {
      WordsCount(line.split(" ").filter(_ != "")
        .groupBy(identity).view.map {el => Count(el._1, el._2.length)}
      .toVector)
    }

    val futureResult = mapReduce(lines)(wordCountFunc)
    Await.result(futureResult, 10.seconds)
  }
}
