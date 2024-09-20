package example

object Main extends App {
  private def sayHello(hello: String, name: String): Unit = {
    println(s"$hello Scala! This is $name")
  }

  val name = "Vanya"
  private val greetings = List("Hello", "Hola", "Guten tag")
  private val names = List(name, name.reverse)

  names.foreach(name => greetings.foreach(greeting => sayHello(greeting, name)))
}
