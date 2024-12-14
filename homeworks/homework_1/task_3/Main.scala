object Main extends App {
  def printHello(greet: String, name: String) = println(s"${greet} Scala! This is ${name}")

  val name = "Vladislav Karamyshev"
  val names = List(name, name.reverse)
  val greetings = List("Hello", "Hola", "Guten tag")


  def foo(x: List[String], y: List[String]): Unit = {
    def bar(x: String, y: List[String]): Unit = {
      if (y.nonEmpty) {
        printHello(x, y.head)
        bar(x, y.tail)
      }
    }

    if (x.nonEmpty) {
      bar(x.head, y)
      foo(x.tail, y)
    }
  }

  foo(greetings, names)
}