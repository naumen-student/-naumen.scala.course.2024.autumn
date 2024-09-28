package example

object Main extends App {
  val name = "Konstantin Zaikin"

  val toGreet = (greeting: String, name: String) => println(s"$greeting Scala! This is $name")

  val greetings = Array("Hello", "Hola", "Guten tag")

  greetings.foreach(greeting => toGreet(greeting, name))
  greetings.foreach(greeting => toGreet(greeting, name.reverse))
}