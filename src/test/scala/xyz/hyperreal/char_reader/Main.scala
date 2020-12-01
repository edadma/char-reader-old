package xyz.hyperreal.char_reader

import Testing._

object Main extends App {

  val s =
    """|a: as #df
    """.trim.stripMargin

  println(text(s))

}
