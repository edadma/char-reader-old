package xyz.hyperreal.char_reader

object Main extends App {

  val s =
    """|1
       | #asdf
       |2
       |""".stripMargin
  val l = CharReader.fromString(s, indentation = Some((Some("#"), None))).toList

  println(l mkString "\n")

}
