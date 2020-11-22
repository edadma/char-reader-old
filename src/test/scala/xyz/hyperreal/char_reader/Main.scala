package xyz.hyperreal.char_reader

import Testing._

object Main extends App {

  val s =
    """|1
       | 2
       | #asdf
       | 3
       |""".stripMargin
  println(indent(s))

}
