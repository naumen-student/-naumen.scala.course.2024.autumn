/*
  Задание №1
  В задание уже описан тайп класс и синтакс для него.
  Вам необходимо в объекте ShowInstance описать инстансы тайп класса
  для типа Cat и Box.
  Тип Cat, в соответствии с тем, какого конкретно наследника этого типа мы хотим показать,
  должен отображаться следующим образом:
  VeryLittleCat - очень маленький кот его_имя
  LittleCat - маленький кот его_имя
  NormalCat - кот его_имя
  BigCat - большой кот его_имя
  VeryBigCat - очень большой кот его_имя

  Если кот будет в коробке, то к тому, что должно выводиться для кота
  необходимо добавить "в коробке". Если коробка пустая, то выводить "пустая коробка"

  В тестах можно всегда более точно посмотреть фразы.
 */
object Task1 extends App {
  // Обратите внимание, что здесь type class параматрезирован контрвариантно
  // благодаря этому инстанс Show[Cat] мы можем применить, например, к BigCat.
  // Однако, это может быть неудобно, так как нам для каждого нового наследника придётся менять
  // реализацию Show[Cat]. Из-за этого практически все библиотеки, которые предоставляют свои
  // тайп классы для их использования пользователем, делают тайп классы инвариантными по параметру.
  // Можете написать две реализации (при -А и А) и сравнить их.
  trait Show[-A] {
    def show(a: A): String
  }

  sealed trait Cat {
    def name: String
  }
  case class VeryLittleCat(name: String) extends Cat
  case class LittleCat(name: String) extends Cat
  case class NormalCat(name: String) extends Cat
  case class BigCat(name: String) extends Cat
  case class VeryBigCat(name: String) extends Cat

  sealed trait Box[+A] {
    def value: A
  }
  case class BoxWith[+A](value: A) extends Box[A]
  case object EmptyBox extends Box[Nothing] {
    override def value: Nothing = throw new Exception("Empty box!")
  }

  object ShowInstance {

    implicit val catShow: Show[Cat] = new Show[Cat] {

      val veryLittleCat = "очень маленький кот"
      val littleCat = "маленький кот"
      val normalCat = "кот"
      val bigCat = "большой кот"
      val veryBigCat = "очень большой кот"
      val inBox = "в коробке"

      override def show(a: Cat): String = {
        a match {
          case _: VeryLittleCat => s"очень маленький кот ${a.name}"
          case _: LittleCat => s"маленький кот ${a.name}"
          case _: NormalCat => s"кот ${a.name}"
          case _: BigCat => s"большой кот ${a.name}"
          case _: VeryBigCat => s"очень большой кот ${a.name}"
          case _ => throw new Exception("Wtf are you doing???")
        }
      }
    }

    import Task1.ShowSyntax._
    implicit def boxShow[A: Show]: Show[Box[A]] = {
      case EmptyBox => "пустая коробка"
      case BoxWith(value) => value.show + " в коробке"
    }
  }

  object ShowSyntax {
    implicit class ShowOps[A](val a: A) {
      def show(implicit show: Show[A]): String = show.show(a)
    }
  }
}