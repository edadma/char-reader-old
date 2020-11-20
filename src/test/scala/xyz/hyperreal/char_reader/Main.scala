package xyz.hyperreal.char_reader

object Main extends App {

  val s =
    """|1
       | 11
       |2
       |22
       |""".stripMargin
  val l = CharReader.fromString(s, indentation = true).toList

  println(l mkString "\n")

}
