import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

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
        items.zipWithIndex.flatMap { case (x, i) => items.zipWithIndex.map { case (y, j) => ((i, j), (x, y)) }.toMap }.map { case (c, e) => (c, e._1 + e._2) }.filter { case (c, e) => e == sumValue && c._1 != c._2 }.lastOption.map(elem => elem._1).getOrElse((-1, -1))
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
    def tailRecRecursion(items: List[Int], index: Int = 1, sum: Int = 0, m: Int = 1): Int = {
        items match {
            case head :: tail =>
                if (head % 2 == 0) {
                    tailRecRecursion(tail, index + 1, sum + m * index, m * head)
                } else {
                    tailRecRecursion(tail, index + 1, sum + m * index, -m * head)
                }
            case _ => sum + m
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
        def RecSearch(start: Int = 0, end: Int = items.length - 1): Option[Int] = {
            if (start > end)
                None
            else if ( end== start) {
                if (items(start) == value)
                    Some(start)
                else
                    None
            }
            else {
                val middle = (start + end + 1) / 2
                if (value < items(middle))
                    RecSearch(start, middle-1)
                else
                    RecSearch(middle, end)
            }
        }

        RecSearch()
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
        val alphabet = 'a' to 'z'
        (for(i <-  1 to namesСount) yield List.fill(scala.util.Random.nextInt(5)+3)(alphabet(scala.util.Random.nextInt(alphabet.size))).mkString.capitalize).toList
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
        def findPhoneNumberSafe(num: String) :Option[String]= Option(unsafePhoneService.findPhoneNumber(num))

        def addPhoneToBaseSafe(phone: String) : Either[Throwable,Unit] ={
            try{
                unsafePhoneService.addPhoneToBase(phone)
                Right()
            }
            catch   {
                case e : InternalError => Left(e)
            }
        }

        def deletePhone(phone: String):Either[Throwable,Unit] = {
            try{
                unsafePhoneService.deletePhone(phone)
                Right()
            }
            catch{
                case e: Throwable => Left(e)
            }
        }
    }

    class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
        override def changePhone(oldPhone: String, newPhone: String): String = {
            val findRes = phoneServiceSafety.findPhoneNumberSafe(oldPhone)
            val res = {
                if (findRes.isDefined) {
                    val deleteRes = phoneServiceSafety.deletePhone(oldPhone)
                    if (deleteRes.isLeft)
                        deleteRes
                }
                phoneServiceSafety.addPhoneToBaseSafe(newPhone)
            }
            res.fold(err => err.getMessage,res=>"ok")
        }
    }
}
