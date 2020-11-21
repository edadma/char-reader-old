package xyz.hyperreal.char_reader

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import xyz.hyperreal.char_reader.Testing._

class BasicIndentationTests extends AnyFreeSpec with Matchers {

  "single line indent" in {
    indent("1\n a\n2") shouldBe
      """
        |<1> (line 1, column 1):
        |1
        |^
        |
        |<\n> (line 1, column 2):
        |1
        | ^
        |
        |<2, 2, INDENT>
        |<a> (line 2, column 2):
        | a
        | ^
        |
        |<\n> (line 2, column 3):
        | a
        |  ^
        |
        |<3, 1, DEDENT>
        |<2> (line 3, column 1):
        |2
        |^
        |""".trim.stripMargin
  }

  "second level indent, then no indent" in {
    indent("1\n a\n  b\n2") shouldBe
      """
        |<1> (line 1, column 1):
        |1
        |^
        |
        |<\n> (line 1, column 2):
        |1
        | ^
        |
        |<2, 2, INDENT>
        |<a> (line 2, column 2):
        | a
        | ^
        |
        |<\n> (line 2, column 3):
        | a
        |  ^
        |
        |<3, 3, INDENT>
        |<b> (line 3, column 3):
        |  b
        |  ^
        |
        |<\n> (line 3, column 4):
        |  b
        |   ^
        |
        |<4, 1, DEDENT>
        |<4, 1, DEDENT>
        |<2> (line 4, column 1):
        |2
        |^
        |""".trim.stripMargin
  }

}
