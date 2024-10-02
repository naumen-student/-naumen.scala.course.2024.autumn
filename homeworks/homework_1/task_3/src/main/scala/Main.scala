object Main {
  def main(args: Array[String]): Unit = {
    val name = "Belousova Anastasia"
    val reversedName = name.reverse

    val hello = (hi: String, name: String) => println(s"$hi Scala! This is $name")

    val greetings = Map("English" -> "Hello", "Spanish" -> "Hola", "French" -> "Bonjour", "Russian" -> "Привет")

    greetings.values.foreach(greeting => hello(greeting, name))
    greetings.values.foreach(greeting => hello(greeting, reversedName))
  }
}