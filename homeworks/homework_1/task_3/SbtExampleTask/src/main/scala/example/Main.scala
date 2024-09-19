package example

object Main extends App{
  private val someName = "Gleb Kochergin"
  private val someNameReversed = someName.reverse
  private val names = List(someName, someNameReversed)
  private val greetings = List("Hello", "Hola", "Guten tag")

  private def greet_name(greeting: String, name: String): Unit = {
    println(s"$greeting Scala! This is $name")
  }

  greetings.foreach(g => names.foreach(n => greet_name(g, n)))
}
