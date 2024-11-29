package example

object Main {
    @main def run()=
      val name = "Lada Shipilovskikh"
      val greet = s"Hello Scala! This is ${name}"
      System.out.println(greet)
      val greetWords = Seq("Hola", "Salut", "Chao")
      for (word <- greetWords) {
        val newGreet = greet.replace("Hello", word)
        System.out.println(newGreet)
      }
      val revName = name.reverse
      val greetWithRevName = s"Hello Scala! This is ${revName}";
      System.out.println(greetWithRevName)
      for (word <- greetWords) {
        val newGreetWithRevName = greetWithRevName.replace("Hello", word)
        System.out.println(newGreetWithRevName)
      }
}
