package xyz.hyperreal.char_reader

object Main extends App {

  val s =
    """|testing
       |1 2 3
       |""".stripMargin
  val l = CharReader.fromString(s).toList

  println(l mkString "\n")

}
