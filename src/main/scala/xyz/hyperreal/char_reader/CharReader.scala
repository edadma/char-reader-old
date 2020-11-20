package xyz.hyperreal.char_reader

import java.io.{FileInputStream, InputStream}

import scala.collection.mutable.ListBuffer

object CharReader {

  val EOI = '\u001A'
  val INDENT = '\uE000'
  val DEDENT = '\uE001'

  def fromString(s: String, tabs: Int = 4, indentation: Boolean = false) = new CharReader(s.iterator, tabs, indentation)

  def fromInputStream(s: InputStream, enc: String, tabs: Int = 4, indentation: Boolean = false): CharReader =
    fromInputStream(s, tabs, indentation)(io.Codec(enc))

  def fromInputStream(s: InputStream, tabs: Int = 4, indentation: Boolean = false)(
      implicit codec: io.Codec): CharReader =
    fromSource(io.Source.fromInputStream(s)(codec), tabs, indentation)

  def fromSource(s: io.Source, tabs: Int = 4, indentation: Boolean = false) = new CharReader(s, tabs, indentation)

  def fromFile(file: String, tabs: Int = 4, indentation: Boolean = false)(implicit codec: io.Codec): CharReader =
    fromInputStream(new FileInputStream(file), tabs, indentation)

  @scala.annotation.tailrec
  private def linelevel(r: CharReader, count: Int = 0): Option[Int] = // not implemented as an instance method because it should be tail recursive
    if (r.more && r.raw.ch == ' ') linelevel(r.next, count + 1)
    else if (r.ch == '\n' || r.eoi) None
    else Some(count)

}

class CharReader private (val list: LazyList[Char],
                          val line: Int,
                          val col: Int,
                          val tabs: Int,
                          val _prev: Char,
                          _start: CharReader,
                          val indentation: Boolean,
                          indent: Int,
                          level: Int) {
  import CharReader.{EOI, INDENT, DEDENT, linelevel}

  def this(it: Iterator[Char], tabs: Int = 4, indentation: Boolean = false) =
    this(it to LazyList, 1, 1, tabs, 0, null, indentation, 0, 0)

  private val start = if (_start eq null) this else _start

  def soi: Boolean = line == 1 && col == 1

  def eoi: Boolean = list.isEmpty

  def more: Boolean = list.nonEmpty

  def ch: Char = if (eoi) EOI else list.head

  lazy val next: CharReader =
    if (indentation && ch == '\n')
      linelevel(this) match {
        case None => raw
        case Some(l) =>
          if (level != 0 && l % indent != 0)
            error(s"expected next indentation level to be ${level + indent} spaces")

          if (l > level)
            new CharReader(list.tail, line + 1, 1, tabs, INDENT, null, indentation, if (indent == 0) l else indent, l)
          else if (l < level)
            new CharReader(list.tail, line + 1, 1, tabs, DEDENT, null, indentation, if (l == 0) 0 else indent, l)
          else
            raw
      } else
      raw

  lazy val raw: CharReader =
    if (eoi)
      eoiError
    else if (ch == '\t')
      new CharReader(list.tail, line, col + (tabs - (col - 1) % tabs), tabs, ch, start, indentation, indent, level)
    else if (ch == '\n')
      new CharReader(list.tail, line + 1, 1, tabs, ch, null, indentation, indent, level)
    else if (ch == INDENT)
      new CharReader(list, line, col, tabs, ch, start, indentation, indent, level)
    else
      new CharReader(list.tail, line, col + 1, tabs, ch, start, indentation, indent, level)

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

    while (r.more && r.ch != '\n') {
      if (r.ch != INDENT && r.ch != DEDENT)
        buf += r.ch

      r = r.next
    }

    buf.toString
  }

  def toList: List[CharReader] = {
    val buf = new ListBuffer[CharReader]
    var r: CharReader = this

    while (r.more) {
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
