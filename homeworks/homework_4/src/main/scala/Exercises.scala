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
        items.zipWithIndex
          .flatMap { case (value1, index1) =>
              items.zipWithIndex
                .filter { case (value2, index2) => value1 + value2 == sumValue && index1 != index2 }
                .map { case (value2, index2) => (index1, index2) }
          }
          .reverse
          .headOption
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

    @tailrec
    def tailRecRecursion(items: List[Int], accValue: Int = 1): Int = {
        if (items.length > 0) {
            tailRecRecursion(
                items.init,
                (if (items.last % 2 == 0) 1 else -1) * items.last * accValue + items.length
            )
        } else {
            accValue
        }
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        def binarySearch(low: Int, high: Int): Option[Int] = {
            if (low > high) {
                None
            } else {
                val mid = low + (high - low) / 2
                items(mid) match {
                    case midValue if midValue == value => Some(mid)
                    case midValue if midValue > value => binarySearch(low, mid - 1)
                    case _ => binarySearch(mid + 1, high)
                }
            }
        }

        binarySearch(0, items.length - 1)
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
        Range(0, namesСount)
          .map { case (nameNumber) =>
              Seq((65 + Random.nextInt(26)).toChar).mkString + Range(0, Random.nextInt(10) + 1).map {
                  case (charNumber) => (97 + Random.nextInt(26)).toChar
              }.mkString
          }
          .toList
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
        def findPhoneNumberSafe(num: String): Option[String] = {
            Option(unsafePhoneService.findPhoneNumber(num))
        }

        def addPhoneToBaseSafe(phone: String): Either[String, String] = {
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right("ok")
            }
            catch {
                case err: Exception => Left(err.getMessage)
            }
        }

        def deletePhone(phone: String): Either[String, String] = {
            try {
                unsafePhoneService.deletePhone(phone)
                Right("ok")
            }
            catch {
                case err: Exception => Left(err.getMessage)
            }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            val oldPhoneRecord = phoneServiceSafety.findPhoneNumberSafe(oldPhone)
            oldPhoneRecord match {
                case Some(phone) => {
                    val oldPhoneDeleted = phoneServiceSafety.deletePhone(phone)
                    oldPhoneDeleted match {
                        case Left(error: String) => error
                        case Right(_) => handlePhoneAdding(phoneServiceSafety, newPhone)
                    }
                }
                case None => handlePhoneAdding(phoneServiceSafety, newPhone)
            }
        }

        def handlePhoneAdding(phoneServiceSafety: PhoneServiceSafety, newPhone: String): String = {
            val newPhoneAdded = phoneServiceSafety.addPhoneToBaseSafe(newPhone)
            newPhoneAdded match {
                case Left(error: String) => error
                case Right(_) => "ok"
            }
        }
    }
}
