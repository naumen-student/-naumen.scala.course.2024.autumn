object Exercises {
    import scala.annotation.tailrec
    import scala.util.Random

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

    def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
        (-1, -1)
        val indices = items.indices.reverse
        val pairs = for {
            i <- indices
            j <- indices
            if i != j && items(i) + items(j) == sumValue
        } yield (i, j)

        pairs.headOption.getOrElse((-1, -1))
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
        def tailRec(items: List[Int], index: Int, acc: Int): Int = {
            items match {
                case head :: tail =>
                    val newAcc = if (head % 2 == 0) head else -head
                    tailRec(tail, index - 1, newAcc * acc + index)
                case _ => acc
            }
        }

        tailRec(items.reverse, items.size, 1)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */
    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        @tailrec
        def binarySearch(low: Int, high: Int): Option[Int] = {
            if (low > high) None
            else {
                val mid = low + (high - low) / 2

                items(mid) match {
                    case midValue if midValue == value => Some(mid)
                    case midValue if midValue < value => binarySearch(mid + 1, high)
                    case _ => binarySearch(low, mid - 1)
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
    def generateNames(namesCount: Int): List[String] = {
        val alphabet = 'a' to 'z'
        val maxLength = 100

        def randomName: String = {
            val firstLetter = alphabet(Random.nextInt(alphabet.size)).toUpper
            val otherLetters = (1 to Random.nextInt(maxLength - 1)).map(_ => alphabet(Random.nextInt(alphabet.size))).mkString
            firstLetter + otherLetters
        }

        List.fill(namesCount)(randomName)
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
        def findPhoneNumberSafe(num: String): Option[String] =
            Option(unsafePhoneService.findPhoneNumber(num))

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] =
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right(())
            } catch {
                case e: Throwable => Left(e.getMessage)
            }

        def deletePhone(phone: String): Either[String, Unit] =
            try {
                unsafePhoneService.deletePhone(phone)
                Right(())
            } catch {
                case e: Throwable => Left(e.getMessage)
            }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Some(_) =>
                    phoneServiceSafety.deletePhone(oldPhone) match {
                        case Right(_) =>
                            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                                case Right(_) => "ok"
                                case Left(error) => s"Error while adding new phone: $error"
                            }
                        case Left(error) => s"Error while deleting old phone: $error"
                    }
                case None => "Phone not found"
            }
        }
    }
}
