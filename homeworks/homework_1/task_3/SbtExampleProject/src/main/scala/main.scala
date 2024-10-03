@main
def main(): Unit = {
  val name = "Sonya"

  val mainPart = "Scala! This is"

  println(s"Hello ${mainPart} ${name}")
  println(s"Hola ${mainPart} ${name}")
  println(s"Guten Tag ${mainPart} ${name}")
  println(s"Hello ${mainPart} ${name.reverse}")
}