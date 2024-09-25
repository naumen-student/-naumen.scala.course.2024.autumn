@main def hello(): Unit =
  val name = "Dmitriy Shilnikov"
  val names = List(name, name.reverse)
  val greetings = List("Hello", "Hola", "Guten tag")

  MakeGreeting(names, greetings)

def hello(name: String, hi: String): Unit = { println(s"${hi} Scala! This is ${name}") }

def MakeGreeting(names: List[String], greetings: List[String]): Unit = {
  names.map(name => greetings.map(greeting => hello(name, greeting)))
}
