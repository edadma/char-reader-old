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

  def soi: Boolean = line == 1 && col == 1

  protected[char_reader] def noneSimple: Boolean

  def next: CharReader

  def nextIgnoreIndentation: CharReader

  def nextSimple: CharReader

  def textUntilDedent(): Unit

  def ch: Char

  def prev: Char

  def substring(end: CharReader): String

  def string(s: String, i: Int = 0): Boolean =
    if (i < s.length)
      if (none || ch != s(i)) false
      else next.string(s, i + 1)
    else
      true

  def skipLine: CharReader =
    if (some && ch != '\n') next.skipLine
    else this //todo: was 'else next'

  @scala.annotation.tailrec
  protected[char_reader] final def lineLevel(indentation: Option[(Option[String], Option[String])],
                                             textUntilDedent: Boolean,
                                             level: Int,
                                             count: Int = 0): Either[CharReader, (Int, CharReader)] =
    if (noneSimple)
      Left(this)
    else if (ch == '\n')
      Left(this) //was nextIgnoreIndentation
    else if (!textUntilDedent && indentation.isDefined && indentation.get._1.isDefined && string(
               indentation.get._1.get))
      Left(skipLine)
    else if (ch == ' ' && (!textUntilDedent || count < level))
      nextIgnoreIndentation.lineLevel(indentation, textUntilDedent, level, count + 1)
    else Right((count, this))

  protected[char_reader] def changeLevel(newindent: Int, newlevel: Int): LazyListCharReader = {
    val (list, tabs, start, indentation, textUntilDedent) =
      this match {
        case l: LazyListCharReader =>
          (l.list, l.tabs, l.start, l.indentation, l._textUntilDedent)
        case _ => (LazyList.empty[Char], 0, null, None, false)
      }

    new LazyListCharReader(list, line, col, tabs, ch, start, indentation, newindent, newlevel, textUntilDedent)
  }

  protected[char_reader] def newline(newindent: Int, newlevel: Int, keepStart: Boolean = false): LazyListCharReader = {
    val (list, newcol, tabs, start, indentation, textUntilDedent) =
      this match {
        case l: LazyListCharReader =>
          (l.list,
           if (keepStart) l.col else 1,
           l.tabs,
           if (keepStart) l.start else null,
           l.indentation,
           l._textUntilDedent)
        case _ => (LazyList.empty[Char], 1, 0, null, None, false)
      }

    new LazyListCharReader(list, line + 1, newcol, tabs, ch, start, indentation, newindent, newlevel, textUntilDedent)
  }

  def longErrorText(msg: String): String

  def error(msg: String): Nothing = sys.error(longErrorText(msg))

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

  def prev: Char = sys.error("can't call prev() method on a SpecialCharReader")

  val some: Boolean = true

  def noneSimple: Boolean = subsequent.none

  def next: CharReader =
    if (count > 1) new SpecialCharReader(ch, count - 1, subsequent)
    else subsequent

  def nextIgnoreIndentation: CharReader = next

  def nextSimple: CharReader = next

  def substring(end: CharReader): String =
    if (this eq end) ""
    else sys.error("non-empty substring() from a SpecialCharReader not implemented")

  def textUntilDedent(): Unit = subsequent.textUntilDedent()

  def longErrorText(msg: String): String = toString
}

class LazyListCharReader private[char_reader] (val list: LazyList[Char],
                                               val line: Int,
                                               val col: Int,
                                               val tabs: Int,
                                               val _prev: Char,
                                               _start: CharReader,
                                               val indentation: Option[(Option[String], Option[String])],
                                               indent: Int,
                                               level: Int,
                                               var _textUntilDedent: Boolean)
    extends CharReader {
  import CharReader._

  def this(it: Iterator[Char], tabs: Int, indentation: Option[(Option[String], Option[String])]) =
    this(it to LazyList, 1, 1, tabs, 0, null, indentation, 0, 0, false)

  def this(list: LazyList[Char], tabs: Int, indentation: Option[(Option[String], Option[String])]) =
    this(list, 1, 1, tabs, 0, null, indentation, 0, 0, false)

  private[char_reader] val start = if (_start eq null) this else _start

  def some: Boolean = list.nonEmpty

  def noneSimple: Boolean = none

  def ch: Char = if (none) EOI else list.head

  lazy val next: CharReader =
    if (indentation.nonEmpty && ch == '\n' && nextIgnoreIndentation.some)
      nextIgnoreIndentation.lineLevel(indentation, _textUntilDedent, level) match {
        case Left(r) => r
        case Right((newlevel, r)) =>
          if (level != 0 && newlevel % indent != 0)
            error(s"expected next indentation level to be ${level + indent} spaces")

          if (newlevel > level)
            new SpecialCharReader(INDENT, 1, r.changeLevel(if (indent == 0) newlevel else indent, newlevel)) //newline(if (indent == 0) l else indent, l, keepStart = true))
          else if (newlevel < level) {
            r match {
              case r: LazyListCharReader => r._textUntilDedent = false
              case _                     =>
            }

            new SpecialCharReader(DEDENT,
                                  (level - newlevel) / indent,
                                  r.changeLevel(if (newlevel == 0) 0 else indent, newlevel))
          } else
            r
      } else
      nextIgnoreIndentation

  lazy val nextIgnoreIndentation: CharReader =
    if (none)
      eoiError
    else if (list.tail.isEmpty && level > 0)
      new SpecialCharReader(DEDENT, level / indent, nextSimple.newline(0, 0))
    else if (ch == '\t')
      new LazyListCharReader(list.tail,
                             line,
                             col + (tabs - (col - 1) % tabs),
                             tabs,
                             ch,
                             start,
                             indentation,
                             indent,
                             level,
                             _textUntilDedent)
    else if (ch == '\n')
      nextSimple
        .newline(indent, level)
    else
      new LazyListCharReader(list.tail, line, col + 1, tabs, ch, start, indentation, indent, level, _textUntilDedent)

  def nextSimple =
    new LazyListCharReader(list.tail, line, col, tabs, ch, start, indentation, indent, level, _textUntilDedent)

  def textUntilDedent(): Unit = _textUntilDedent = true

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

  def toListIndentedText: List[CharReader] = {
    val buf = new ListBuffer[CharReader]
    var r: CharReader = this

    while (r.some) {
      if (r.ch == INDENT)
        r.textUntilDedent()

      buf += r
      r = r.next
    }

    buf.toList
  }

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
