
@main
def main(): Unit = {
  def sayHello(start: String, name: String) = println(s"$start Scala! This is $name")

  val myName = "Artem Sorokin"
  val names = List(myName, myName.reverse)
  val hellos = List("Hello", "Hola", "Hi")

  for (hello <- hellos)
    for(name <- names) sayHello(hello, name)
}

