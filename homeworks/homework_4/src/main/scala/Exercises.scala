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

    def findSumFunctional(items: List[Int], sumValue: Int) = {
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

    def tailRecRecursion(items: List[Int]): Int = {
        items.zipWithIndex.foldRight(1) {
            case ((v, index), acc) =>
                if (v % 2 == 0) {
                    v * acc + index + 1
                } else {
                    -1 * v * acc + index + 1
                }
        }
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def binarySearchHelper(low: Int, high: Int): Option[Int] = {
            if (low > high) {
                None
            } else {
                val mid = low + (high - low) / 2
                items(mid) match {
                    case midValue if midValue == value => Some(mid)
                    case midValue if midValue < value => binarySearchHelper(mid + 1, high)
                    case _ => binarySearchHelper(low, mid - 1)
                }
            }
        }

        binarySearchHelper(0, items.length - 1)
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesСount: Int): List[String] = {
        if (namesСount < 0) throw new Throwable("Invalid namesCount")

        val alphabet = 'A' to 'Z'

        def generateName: String = {
            val length = Random.nextInt(20) + 3
            val name = (1 to length).map(_ => alphabet(Random.nextInt(alphabet.size))).mkString
            name.head + name.tail.toLowerCase
        }

        List.fill(namesСount)(generateName)
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
    import scala.util.{Try, Success, Failure}

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
        def findPhoneNumberSafe(num: String): Option[String] = {
            Option(unsafePhoneService.findPhoneNumber(num))
        }

        def addPhoneToBaseSafe(phone: String): Either[String, String] = {
            Try(unsafePhoneService.addPhoneToBase(phone)) match {
                case Success(_) => Right("ok")
                case Failure(exception) => Left(exception.getMessage)
            }
        }

        def deletePhoneSafe(phone: String): Either[String, String] = {
            Try(unsafePhoneService.deletePhone(phone)) match {
                case Success(_) => Right("ok")
                case Failure(exception) => Left(exception.getMessage)
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
                                case Left(error) => error
                            }
                        case Left(error) => error
                    }
                case None => "Old phone number not found"
            }
        }
    }
}
