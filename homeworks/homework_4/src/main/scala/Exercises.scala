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

    def findSumFunctional(items: List[Int], sumValue: Int) : (Int, Int) = {
        items.zipWithIndex.flatMap {
            case (item1, i) => items.zipWithIndex.find {
                case (item2, j) => item1 + item2 == sumValue && i != j
            }.map { case (_, j) => (i, j) }
        }
          .lastOption
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
    def tailRecRecursion(items: List[Int], acc: Int = 1): Int = {
        if (items.isEmpty)
            acc
        else {
            val pref = if (items.last % 2 == 0) 1 else -1
            val index = items.size
            tailRecRecursion(items.init, pref * items.last * acc + index)
        }

    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        binarySearchRecursive(items, value, 0, items.size - 1)
    }

    @tailrec
    def binarySearchRecursive(items: List[Int], value: Int, left: Int, right: Int) : Option[Int] = {
        val mid  = left + (right - left) / 2

        if (left > right)
            None
        else if (items(mid) == value)
            Option(mid)
        else {
            val nextLeft = if (items(mid) < value) mid + 1 else left
            val nextRight = if (items(mid) > value) mid - 1 else right
            binarySearchRecursive(items, value, nextLeft, nextRight)
        }
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesCount: Int): List[String] = {
        if (namesCount < 0) throw new Throwable("Invalid namesCount")

        genearateUniqueNames(namesCount).toList
    }

    @tailrec
    def getRandomNameRecursive(length: Int, collected: Seq[Char] = Seq.empty): String = {
        if (length == 0)
             collected.mkString.capitalize
        else
            getRandomNameRecursive(length - 1, collected :+ (Random.nextInt(122 - 97) + 97).toChar)
    }

    @tailrec
    def genearateUniqueNames(namesCount: Int, acc: Set[String] = Set.empty) : Set[String] = {
        if (acc.size == namesCount)
            acc
        else
            genearateUniqueNames(namesCount, acc + getRandomNameRecursive(Random.nextInt(99) + 1))
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
            phoneServiceSafety.findPhoneNumberSafe(oldPhone) match {
                case Some(oldPhone) => phoneServiceSafety.deletePhone(oldPhone) match {
                    case Left(err) => s"An error occurred: $err"
                    case Right(_) => phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                        case Left(err) => s"An error occurred: $err"
                        case Right(_) => "ok"
                    }
                }
                case None => phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                    case Left(err) => err
                    case Right(_) => "ok"
                }
            }
        }
    }
}
