object Main extends App{
  val name = "Max Chevychelov"
  val reversedName = name.reverse
  val greetings = List("Hello", "Hola", "Guten tag")

  def writeGreetings(greeting: String, name: String): Unit = {
    println(s"$greeting Scala! This is $name")
  }

  greetings.foreach(greeting => writeGreetings(greeting, name))

  greetings.foreach(greeting => writeGreetings(greeting, reversedName))
}
