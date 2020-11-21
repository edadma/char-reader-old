package xyz.hyperreal.char_reader

object Main extends App {

  val s =
    """|1
       |
       | a
       |2
       | b
       |
       |3
       | c
       |""".stripMargin
  val l = CharReader.fromString(s, indentation = true).toList

  println(l mkString "\n")

}
