package xyz.hyperreal.char_reader

object Main extends App {

  val s = "a\n b\n"
  val l = CharReader.fromString(s, indentation = Some((None, None))).toList

  println(l mkString "\n")

}
