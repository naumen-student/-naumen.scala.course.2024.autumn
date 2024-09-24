def hello(greeting: String, name: String): Unit = {
  println(greeting + " Scala! This is " + name)
}

@main
def main(): Unit = {
  val greeting = "Hello"
  val name = "Sasha"

  hello(greeting, name)

  val greeting1 = "Привет"

  hello(greeting1, name)

  val greeting2 = "Guten tag"
  val reversedName = name.reverse

  hello(greeting2, reversedName)
}




