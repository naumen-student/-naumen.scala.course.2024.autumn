package hw1

object Main extends App {
  def greet(greeting: String, name: String): Unit = {
    println(s"$greeting Scala! This is $name")
  }

  val name = "Anna Vasileva"
  val names = List(name, name.reverse)
  val greetings = List("Hello", "Hola", "Guten tag")

  greetings.foreach(g => names.foreach(n => greet(g, n)))
}
