package example

object Main extends App {
  val name = "Arseniy Yolkin"
  val reversedName = name.reverse
  val greetings = List("Hello", "Hola", "Guten tag")
  val names = List(name, reversedName)

  greetings.foreach(greeting => names.foreach(name => greetScala(name, greeting)))

  def greetScala(name: String, greeting: String): Unit = {
    println(s"$greeting Scala! This is $name")
  }
}
