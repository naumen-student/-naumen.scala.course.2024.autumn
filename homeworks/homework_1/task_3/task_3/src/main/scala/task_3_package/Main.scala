object Main extends App {
  def greetings(greetingsWord: String, name: String): Unit = {
    println(s"$greetingsWord Scala! This is $name")
  }

  val myName = "Ilya Kuksovsky"
  val greetingsWords = List("Hello", "Hola", "Guten tag")
  greetingsWords.foreach(x => greetings(x, myName))
  greetingsWords.foreach(x => greetings(x, myName.reverse))
}