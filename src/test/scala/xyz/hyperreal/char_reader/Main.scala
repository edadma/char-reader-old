package xyz.hyperreal.char_reader

object Main extends App {

  val s = "1\n a\n2"
//  val s = "1\n a \n  "
//  val s = "1\n a\n  "
//  val s = "1\n a\n  \n"
//  val s = "1\n "
//  val s =
//    """|0
//       | 1
//       |  a
//       |  b
//       | 2
//       |3
//       |""".stripMargin
  val l = CharReader.fromString(s, indentation = Some((Some("#"), None))).toList

  println(l map (r =>
    r.longErrorText(s"<${r.ch match {
      case '\n' => "\\n"
      case c    => c.toString
    }}>")) mkString "\n")

}
