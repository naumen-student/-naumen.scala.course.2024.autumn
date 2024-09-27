package example

object Main extends App {
  val name = "Kolya Mozganov"

  val createNamedGreeting = (greeting: String, name: String) => s"$greeting Scala! This is $name"

  val greetings = List("Hello", "Hola", "Guten Tag")

  greetings.foreach(g => println(createNamedGreeting(g, name)))

  val reversedName = name.reverse

  greetings.foreach(g => println(createNamedGreeting(g, reversedName)))
}
