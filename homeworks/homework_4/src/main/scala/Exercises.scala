import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}

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
        for (i <- items.indices) {
            for (j <- items.indices) {
                if (items(i) + items(j) == sumValue && i != j) {
                    result = (i, j)
                }
            }
        }
        result
    }

    def findSumFunctional(items: List[Int], sumValue: Int) = {
        val indexedItems = items.zipWithIndex

        val result = for {
            (value1, index1) <- indexedItems
            (value2, index2) <- indexedItems if index1 != index2 && value1 + value2 == sumValue
        } yield (index1, index2)

        result.headOption.getOrElse((-1, -1))
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
        @tailrec
        def tailInner(remaining: List[Int], index: Int, accumulator: Int): Int = {
            remaining match {
                case head :: tail =>
                    val currentResult = if (head % 2 == 0) {
                        head * accumulator + index
                    } else {
                        -1 * head * accumulator + index
                    }
                    tailInner(tail, index + 1, currentResult)
                case Nil => accumulator
            }
        }

        tailInner(items, 1, 1)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def searchInner(low: Int, high: Int): Option[Int] = {
            if (low > high) {
                None
            } else {
                val mid = low + (high - low) / 2
                items(mid) match {
                    case x if x == value => Some(mid)
                    case x if x < value => searchInner(mid + 1, high)
                    case _ => searchInner(low, mid - 1)
                }
            }
        }

        searchInner(0, items.length - 1)
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesCount: Int): List[String] = {
        val random = new Random()

        def generateName: String = {
            val length = random.nextInt(5) + 3
            val name = (1 to length).map { i =>
                if (i == 1) {
                    ('A' + random.nextInt(26)).toChar
                } else {
                    ('a' + random.nextInt(26)).toChar
                }
            }.mkString("")
            name
        }

        def generateNamesRec(count: Int): List[String] = {
            if (count == 0) Nil
            else generateName :: generateNamesRec(count - 1)
        }

        generateNamesRec(namesCount)
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
        def findPhoneNumberSafe(num: String): Option[String] = Some(unsafePhoneService.findPhoneNumber(num))

        def handleError(func: () => Unit): Either[InternalError, String] = {
            val res = Try {
                func()
                "success"
            }

            res match {
                case Success(value) => Right(value)
                case Failure(exception) => Left(new InternalError(exception.getMessage))
            }
        }

        def addPhoneToBaseSafe(phone: String): Either[InternalError, String] =
             handleError(() => unsafePhoneService.addPhoneToBase(phone))

        def deletePhone(phone: String): Either[InternalError, String] =
             handleError(() => unsafePhoneService.addPhoneToBase(phone))

    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Some(oldPhoneRecord) =>
                    phoneServiceSafety.deletePhone(oldPhoneRecord) match {
                        case Right(_) =>
                            tryAddPhone(newPhone)
                        case Left(error) => error.getMessage
                    }
                case None =>
                    tryAddPhone(newPhone)
            }
        }

        def tryAddPhone(newPhone: String): String = {
            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Right(_) => "success"
                case Left(error) => error.getMessage
            }
        }
    }
}
