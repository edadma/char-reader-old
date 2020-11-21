package xyz.hyperreal.char_reader

object Main extends App {

  val s =
    """|1
       | a
       | b
       |2
       |""".stripMargin
  val l = CharReader.fromString(s, indentation = Some((Some("#"), None))).toList

  println(l map (r => r.longErrorText(s"<${r.ch}")) mkString "\n")

}
