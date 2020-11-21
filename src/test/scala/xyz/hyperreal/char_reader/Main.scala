package xyz.hyperreal.char_reader

import Testing._

object Main extends App {

  val s = "1\n a \n  "
//  val s = "1\n a\n  "
//  val s = "1\n a\n  \n"
//  val s =
//    """|1
//       | 2
//       |  a
//       |
//       |  b
//       | 3
//       |4
//       |""".stripMargin
//  val s = "1\n \n"

  println(indent(s))

}
