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

	def findSumFunctional(items: List[Int], sumValue: Int): (Int, Int) = {
		items.zipWithIndex
			.flatMap { case (item1, index1) =>
				items.zipWithIndex
					.filter { case (item2, index2) => item1 + item2 == sumValue && index1 != index2 }
					.map { case (_, index2) => (index1, index2) }
			}.lastOption
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
		@tailrec
		def tailRecRecursionInner(itemsInner: List[Int], index: Int, accumulator: Int): Int = {
			itemsInner match {
				case head :: tail =>
					if (head % 2 == 0) {
						tailRecRecursionInner(tail, index - 1, accumulator * head + index)
					} else {
						tailRecRecursionInner(tail, index - 1, accumulator * -1 * head + index)
					}
				case _ => accumulator
			}
		}

		tailRecRecursionInner(items reverse, items.length, 1)
	}

	/**
	 * Задание №3
	 * Реализуйте алгоритм бинарного поиска, который соответсвует всем правилам функционального программирования.
	 * Необходимо возвращать индекс соответствующего элемента в массиве
	 * Если ответ найден, то возвращается Some(index), если нет, то None
	 */

	//	def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
	//		None
	//	}


	def functionalBinarySearch(items: List[Int], value: Int): Option[Int] = {
		@tailrec
		def functionalBinarySearchInner(left: Int, right: Int): Option[Int] = {
			if (left >= right) {
				return None
			}

			val mid = left + (right - left) / 2
			items(mid).compareTo(value) match {
				case 0 => Some(mid)
				case -1 => functionalBinarySearchInner(mid + 1, right)
				case 1 => functionalBinarySearchInner(left, mid)
			}
		}

		functionalBinarySearchInner(0, items.length)
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

		val bigLetters = ('A' to 'Z').toList
		val lowLetters = ('a' to 'z').toList

		(1 to namesСount)
			.map(_ => bigLetters(Random.nextInt(26))
				+ (1 to Random.nextInt(5) + 5).map(_ => lowLetters(Random.nextInt(26))).mkString
			)
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
			val phoneNumber = unsafePhoneService.findPhoneNumber(num)
			phoneNumber match {
				case null => None
				case _ => Some(phoneNumber)
			}
		}

		def addPhoneToBaseSafe(phone: String): Try[Unit] = {
			Try(unsafePhoneService.addPhoneToBase(phone))
		}

		def deletePhone(phone: String): Unit = {
			unsafePhoneService.deletePhone(phone)
		}
	}

	class ChangePhoneServiceSafe(phoneServiceSafety: PhoneServiceSafety) extends ChangePhoneService {
		override def changePhone(oldPhone: String, newPhone: String): String = {
			phoneServiceSafety.findPhoneNumberSafe(oldPhone).foreach(phoneServiceSafety.deletePhone)
			phoneServiceSafety.addPhoneToBaseSafe(newPhone) match {
				case Failure(_) => "failure"
				case Success(_) => "ok"
			}
		}
	}
}
