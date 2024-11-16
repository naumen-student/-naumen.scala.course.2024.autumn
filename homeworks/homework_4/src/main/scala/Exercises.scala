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

    def findSumFunctional(items: List[Int], sumValue: Int) = 
        items
        .zipWithIndex
        .map(value => (items.indexWhere(_ == sumValue - value._1, value._2 - 1), value._2))
        .filter(returnValue => returnValue._1 != -1 && returnValue._1 != returnValue._2)
        .find(_ => true)
        .getOrElse((-1, -1))

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
        def recursion(itemsRecursion: List[Int], index: Int, result: Int): Int = {
            itemsRecursion match {
                case Nil => result
                case head :: tail =>
                    if (head % 2 == 0) {
                        recursion(tail, index - 1, head * result + index)
                    } else {
                        recursion(tail, index - 1, -1 * head * result + index)
                    }

            }
        }

        recursion(items.reverse, items.length, 1)
    }

    /**
     * Задание №3
     * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
     * Необходимо возвращать индекс соответствующего элемента в массиве
     * Если ответ найден, то возвращается Some(index), если нет, то None
     */

    def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
        
        @tailrec
        def tailRecBinarySearch(itemsRecurcive: List[Int], leftBound: Int, rightBound: Int, foundNum: Int): Option[Int] = {
            var mid = (leftBound + rightBound) / 2
            itemsRecurcive(mid) match {
                case `foundNum` => Some(mid)
                case _ => leftBound > rightBound match {
                    case true => None
                    case false => if (itemsRecurcive(mid) < foundNum) {
                        tailRecBinarySearch(itemsRecurcive, mid + 1, rightBound, foundNum)
                    } else {
                        tailRecBinarySearch(itemsRecurcive, leftBound, mid - 1, foundNum)
                    }
                }
            }
        }
        if (items.length > 0) {
            tailRecBinarySearch(items, 0, items.length - 1, value)
        } else {
            None
        }
    }

    /**
     * Задание №4
     * Реализуйте функцию, которая генерирует список заданной длинны c именами.
     * Функция должна соответствовать всем правилам функционального программирования.
     *
     * Именем является строка, не содержащая иных символов, кроме буквенных, а также начинающаяся с заглавной буквы.
     */

    def generateNames(namesСount: Int): List[String] = {
        val chars: IndexedSeq[Char] = ('a' to 'z')
        if (namesСount < 0) throw new Throwable("Invalid namesCount")
        if (namesСount == 0) {
            Nil
        } else {
           List.fill(namesСount)(Random.shuffle(chars).head.toUpper.toString() + Random.shuffle(chars).take(2 + Random.nextInt(10)).mkString)
        }
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
        def findPhoneNumberSafe(num: String): Option[string] = Option(unsafePhoneService.findPhoneNumber(num))

        def addPhoneToBaseSafe(phone: String): Either[String, Unit] = 
            if (checkPhoneNumber(phone)) {
                Right(unsafePhoneService.addPhoneToBase(phone))
            } else {
                Left("Invalid phone string")
            } 

        def deletePhone(phone: String): Either[String, Unit] =
            findPhoneNumberSafe(phone) match {
                case Some(num) => Right(unsafePhoneService.deletePhone(num))
                case _ => Left("Phone not found")
            }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            phoneServiceSafety.findPhoneNumberSafe(oldPhone).map(phoneServiceSafety.deletePhone)
            phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
                case Right(_) => "ok"
                case Left(fault) => fault
            }
        }
    }
}
