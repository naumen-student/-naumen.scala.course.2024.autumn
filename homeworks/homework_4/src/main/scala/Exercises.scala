import scala.annotation.tailrec

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
        val pairs = for {
            i <- items.indices
            j <- items.indices
            if i != j && items(i) + items(j) == sumValue
        } yield (i, j)
        if (pairs.isEmpty) (-1, -1) else pairs.last
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
    def tailRecRecursion(items: List[Int], index: Int = 1, acc: Int = 1): Int = {
        items match {
            case head :: tail =>
                val newAcc = if (head % 2 == 0) head * acc + index else -head * acc + index
                tailRecRecursion(tail, index + 1, newAcc)
            case Nil => acc
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
        def binarySearch(items: List[Int], value: Int, low: Int, high: Int): Option[Int] = {
            if (low > high) {
                None
            } else {
                val mid = low + (high - low) / 2
                items.lift(mid) match {
                    case Some(midVal) =>
                        if (midVal == value) Some(mid)
                        else if (midVal < value) binarySearch(items, value, mid + 1, high)
                        else binarySearch(items, value, low, mid - 1)
                    case None => None
                }
            }
        }
        binarySearch(items, value, 0, items.length - 1)
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

        def indexToName(index: Int): String = {
            @tailrec
            def helper(i: Int, acc: String): String = {
                if (i < 0) acc
                else {
                    val remainder = i % 26
                    val char = ('A' + remainder).toChar
                    helper((i / 26) - 1, char + acc)
                }
            }
            helper(index, "")
        }

        (0 until namesCount).toList.map(i => s"Name${indexToName(i)}")
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
            Some(unsafePhoneService.findPhoneNumber(num))
        }

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] = {
            try {
                unsafePhoneService.addPhoneToBase(phone)
                Right(())
            } catch {
                case error: InternalError => Left(s"Failed to add phone: ${error}")
            }
        }

        def deletePhoneSafe(phoneRecord: String): Either[String, Unit] = {
            try {
                unsafePhoneService.deletePhone(phoneRecord)
                Right(())
            } catch {
                case error: Error => Left(s"Failed to delete phone: ${error}")
            }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Some(oldPhoneRecord) =>
                    phoneServiceSafety.deletePhoneSafe(oldPhoneRecord) match {
                        case Right(_) =>
                            tryAddPhoneToBase(newPhone)
                        case Left(error) => error
                    }
                case None =>
                    tryAddPhoneToBase(newPhone)
            }
        }

        private def tryAddPhoneToBase(newPhone: String) = {
            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Right(_) => "ok"
                case Left(error) => error
            }
        }
    }
}
