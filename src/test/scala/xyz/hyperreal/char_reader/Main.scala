package xyz.hyperreal.char_reader

import Testing._

object Main extends App {

  val s =
    """|1
       | a
       |  b
       | c
       |2
       | d
       |""".stripMargin
  println(text(s))

}
