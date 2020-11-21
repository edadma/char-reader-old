package xyz.hyperreal.char_reader

import java.io.{FileInputStream, InputStream}

import scala.collection.mutable.ListBuffer

object CharReader {

  val EOI = '\u001A'
  val INDENT = '\uE000'
  val DEDENT = '\uE001'

  def fromString(s: String, tabs: Int = 4, indentation: Option[(Option[String], Option[String])] = None) =
    new LazyListCharReader(s.iterator, tabs, indentation)

  def fromInputStream(s: InputStream, tabs: Int = 4, indentation: Option[(Option[String], Option[String])] = None)(
      implicit codec: io.Codec): CharReader =
    fromSource(io.Source.fromInputStream(s)(codec), tabs, indentation)

  def fromSource(s: io.Source, tabs: Int = 4, indentation: Option[(Option[String], Option[String])] = None) =
    new LazyListCharReader(s, tabs, indentation)

  def fromFile(file: String, tabs: Int = 4, indentation: Option[(Option[String], Option[String])] = None)(
      implicit codec: io.Codec): CharReader =
    fromInputStream(new FileInputStream(file), tabs, indentation)

}

abstract class CharReader {
  import CharReader.{INDENT, DEDENT}

  def line: Int

  def col: Int

  def some: Boolean

  def none: Boolean = !some

  def next: CharReader

  def raw: CharReader

  def ch: Char

  override def toString =
    s"<$line, $col, ${if (ch >= ' ' && ch <= '~') ch.toString
    else
      ch match {
        case INDENT => "INDENT"
        case DEDENT => "DEDENT"
        case '\n'   => "\\n"
        case _      => "?"
      }}>"
}

class SpecialCharReader(val ch: Char, count: Int, subsequent: CharReader) extends CharReader {
  def line: Int = subsequent.line

  def col: Int = subsequent.col

  val some: Boolean = true

  def next: CharReader =
    if (count > 1) new SpecialCharReader(ch, count - 1, subsequent)
    else subsequent

  def raw: CharReader = next

}

object LazyListCharReader {

  @scala.annotation.tailrec
  private def linelevel(r: CharReader, count: Int = 0): Option[Int] = // not implemented as an instance method because it should be tail recursive
    if (r.none || r.ch == '\n') None
    else if (r.ch == ' ') linelevel(r.raw, count + 1)
    else Some(count)

}

class LazyListCharReader private (val list: LazyList[Char],
                                  val line: Int,
                                  val col: Int,
                                  val tabs: Int,
                                  val _prev: Char,
                                  _start: CharReader,
                                  val indentation: Option[(Option[String], Option[String])],
                                  indent: Int,
                                  level: Int)
    extends CharReader {
  import CharReader._
  import LazyListCharReader._

  def this(it: Iterator[Char], tabs: Int, indentation: Option[(Option[String], Option[String])]) =
    this(it to LazyList, 1, 1, tabs, 0, null, indentation, 0, 0)

  def this(list: LazyList[Char], tabs: Int, indentation: Option[(Option[String], Option[String])]) =
    this(list, 1, 1, tabs, 0, null, indentation, 0, 0)

  private val start = if (_start eq null) this else _start

  def soi: Boolean = line == 1 && col == 1

  def some: Boolean = list.nonEmpty

  def ch: Char = if (none) EOI else list.head

  private def newline(newindent: Int, newlevel: Int) =
    new LazyListCharReader(if (list.isEmpty) list else list.tail,
                           line + 1,
                           1,
                           tabs,
                           ch,
                           null,
                           indentation,
                           newindent,
                           newlevel)

  lazy val next: CharReader =
    if (indentation.nonEmpty && ch == '\n' && raw.some)
      linelevel(raw) match {
        case None => raw
        case Some(l) =>
          if (level != 0 && l % indent != 0)
            error(s"expected next indentation level to be ${level + indent} spaces")

          if (l > level)
            new SpecialCharReader(INDENT, 1, newline(if (indent == 0) l else indent, l))
          else if (l < level)
            new SpecialCharReader(DEDENT, (level - l) / indent, newline(if (l == 0) 0 else indent, l))
          else
            raw
      } else
      raw

  lazy val raw: CharReader =
    if (none)
      eoiError
    else if (list.tail.isEmpty && level > 0)
      new SpecialCharReader(DEDENT, level / indent, newline(0, 0))
    else if (ch == '\t')
      new LazyListCharReader(list.tail,
                             line,
                             col + (tabs - (col - 1) % tabs),
                             tabs,
                             ch,
                             start,
                             indentation,
                             indent,
                             level)
    else if (ch == '\n')
      newline(indent, level)
    else
      new LazyListCharReader(list.tail, line, col + 1, tabs, ch, start, indentation, indent, level)

  lazy val prev: Char =
    if (soi)
      error("no previous character")
    else
      _prev

  def before(that: CharReader): Boolean = line < that.line || (line == that.line && col < that.col)

  def substring(end: CharReader): String = {
    val buf = new StringBuilder
    var r: CharReader = this

    while (r ne end) {
      if (r.ch != INDENT && r.ch != DEDENT)
        buf += r.ch

      r = r.next
    }

    buf.toString
  }

  def lineString: String = {
    val buf = new StringBuilder
    var r: CharReader = this.start

    while (r.some && r.ch != '\n') {
      if (r.ch != INDENT && r.ch != DEDENT)
        buf += r.ch

      r = r.next
    }

    buf.toString
  }

  def toList: List[CharReader] = {
    val buf = new ListBuffer[CharReader]
    var r: CharReader = this

    while (r.some) {
      buf += r
      r = r.next
    }

    buf.toList
  }

  def error(msg: String): Nothing = sys.error(longErrorText(msg))

  def eoiError: Nothing = error("end of input")

  def lineText: String = {
    val buf = new StringBuilder
    var zcol = 0

    lineString foreach {
      case '\t' =>
        val len = tabs - zcol % tabs

        buf ++= " " * len
        zcol += len
      case '\n' => sys.error("found newline in string from lineString()")
      case c =>
        buf += c
        zcol += 1
    }

    buf.toString
  }

  def errorText: String = lineText + '\n' + (" " * (col - 1)) + "^\n"

  def longErrorText(msg: String): String = s"$msg (line $line, column $col):\n" + errorText

}
