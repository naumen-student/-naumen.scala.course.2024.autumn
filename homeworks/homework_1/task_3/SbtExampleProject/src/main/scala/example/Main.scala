package example

object Main extends App {
  val name = "Lada Kuzmina"
  private val reversedName = name.reverse
  private val greetings = List("Hello", "Hola", "Guten tag")

  private def printGreeting(greeting: String, name: String): Unit = {
    println(s"$greeting Scala! This is $name")
  }

  greetings.foreach(greeting => printGreeting(greeting, name))
  greetings.foreach(greeting => printGreeting(greeting, reversedName))
}
