package xyz.hyperreal.char_reader

import Testing._

object Main extends App {

  val s =
    """|1
       | 2
       |  a
       |  b
       | 3
       |4
       |""".stripMargin

  println(indent(s))

}
