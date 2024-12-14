import scala.annotation.tailrec
import scala.util.Random

object Exercises {

  /**
   * Задание №1
   * Дана императивная функция findSumImperative.
   * Напишите ее аналог (findSumFunctional) в функциональном стиле.
   *
   * ПОДСКАЗКА
   * Стоит воспользоваться методами, которые предоставляет объект List или рекурсией.
   * Страница с полезностями List: https://alvinalexander.com/scala/list-class-methods-examples-syntax/
   */
  def findSumImperative(items: List[Int], sumValue: Int): (Int, Int) = {
    var result: (Int, Int) = (-1, -1)
    for (i <- 0 until items.length) {
      for (j <- 0 until items.length) {
        if (items(i) + items(j) == sumValue && i != j) {
          result = (i, j)
        }
      }
    }
    result
  }

  def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
    items.zipWithIndex.combinations(2)
      .collectFirst {
        case List((a, j), (b, i)) if a + b == sumValue => (i, j)
      }
      .getOrElse((-1, -1))
  }


  /**
   * Задание №2
   *
   * Дана рекурсивная функция simpleRecursion.
   * Перепишите ее так, чтобы получилась хвостовая рекурсивная функция.
   *
   * Для прохождения теста на большое количество элементов в списке
   * используйте анотацию @tailrec к вашей функции.
   */
  def simpleRecursion(items: List[Int], index: Int = 1): Int = {
    items match {
      case head :: tail =>
        if (head % 2 == 0) {
          head * simpleRecursion(tail, index + 1) + index
        } else {
          -1 * head * simpleRecursion(tail, index + 1) + index
        }
      case _ => 1
    }
  }

  def tailRecRecursion(items: List[Int], accumulator: Int = 1, index: Int = 1): Int = {
    @tailrec
    def run(items: List[Int], index: Int, acc: Int = 1): Int = {
      items match {
        case Nil => acc
        case head :: tail =>
          if (head % 2 == 0) run(tail, index - 1, head * acc + index)
          else run(tail, index - 1, -head * acc + index)
      }
    }

    run(items.reverse, items.size)

  }

  /**
   * Задание №3
   * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
   * Необходимо возвращать индекс соответствующего элемента в массиве
   * Если ответ найден, то возвращается Some(index), если нет, то None
   */

  def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
    @tailrec
    def search(list: List[Int], value: Int, left: Int, right: Int): Option[Int] = {
      if (left > right) None
      else {
        val middle = left + (right - left) / 2
        list(middle) match {
          case x if x == value => Some(middle)
          case x if x < value => search(list, value, middle + 1, right)
          case x if x > value => search(list, value, left, middle - 1)
        }
      }
    }

    if (items.isEmpty) None
    else search(items, value, 0, items.size - 1)
  }

  /**
   * Задание №4
   * Реализуйте функцию, которая генерирует список заданной длинны c именами.
   * Функция должна соответствовать всем правилам функционального программирования.
   *
   * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
   */

  def generateNames(namesCount: Int): List[String] = {
    if (namesCount < 0) throw new IllegalArgumentException("Invalid namesCount")

    def generateName(): String = {
      val alphabet = ('A' to 'Z') ++ ('a' to 'z')
      val randomLength = Random.nextInt(10) + 3
      val name = (for (_ <- 1 to randomLength) yield alphabet(Random.nextInt(alphabet.size))).mkString
      name.head.toUpper + name.tail.toLowerCase
    }

    (1 to namesCount).map(_ => generateName()).toList
  }

}

/**
 * Задание №5
 *
 * Дана реализация сервиса по смене номера SimpleChangePhoneService с методом changePhone
 * Необходимо написать реализацию этого сервиса с учетом правил работы со сторонними эффектами (SideEffects).
 *
 * Для этого необходимо сначала реализовать собственный сервис работы с телефонными номерами (PhoneServiceSafety),
 * используя при этом методы из unsafePhoneService.
 * Методы должны быть безопасными, поэтому тип возвращаемых значений необходимо определить самостоятельно.
 * Рекомендуется воспользоваться стандартными типами Scala (например Option или Either).
 *
 * Затем, с использованием нового сервиса, необходимо реализовать "безопасную" версию функции changePhone.
 * Функция должна возвращать ok в случае успешного завершения или текст ошибки.
 *
 * Изменять методы внутри SimplePhoneService не разрешается.
 */

object SideEffectExercise {

  import Utils._

  class SimpleChangePhoneService(phoneService: SimplePhoneService) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      val oldPhoneRecord = phoneService.findPhoneNumber(oldPhone)
      if (oldPhoneRecord != null) {
        phoneService.deletePhone(oldPhoneRecord)
      }
      phoneService.addPhoneToBase(newPhone)
      "ok"
    }
  }


  class PhoneServiceSafety(unsafePhoneService: SimplePhoneService) {

    import Utils._

    def findPhoneNumberSafe(num: String): Option[String] = {
      Option(unsafePhoneService.findPhoneNumber(num))
    }

    def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
      if (checkPhoneNumber(phone)) {
        try {
          unsafePhoneService.addPhoneToBase(phone)
          Right(())
        } catch {
          case e: Exception => Left(e.getMessage)
        }
      } else {
        Left("Invalid phone string")
      }
    }

    def deletePhoneSafe(phone: String): Either[String, Unit] = {
      try {
        unsafePhoneService.deletePhone(phone)
        Right(())
      } catch {
        case e: Exception => Left(e.getMessage)
      }
    }
  }

  class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
    override def changePhone(oldPhone: String, newPhone: String): String = {
      phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
        case Some(_) =>
          phoneServiceSafety.deletePhoneSafe(oldPhone) match {
            case Right(_) =>
              phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Right(_) => "ok"
                case Left(error) => s"Error adding new phone: $error"
              }
            case Left(error) => s"Error deleting old phone: $error"
          }
        case None =>
          phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
            case Right(_) => "ok"
            case Left(error) => s"Error adding new phone: $error"
          }
      }
    }
  }
}
