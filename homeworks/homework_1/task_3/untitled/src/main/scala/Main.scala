object Main extends App {
  val name = "Nikolay Tikhonchik"
  val greeting = s"Hello Scala! This is $name"
  val greetings = List("Hola", "Guten tag")

  def reverseName(name: String): String = name.reverse

  println(greeting)

  greetings.foreach(greet => println(s"$greet Scala! This is $name"))

  greetings.foreach(greet => println(s"$greet Scala! This is ${reverseName(name)}"))
}