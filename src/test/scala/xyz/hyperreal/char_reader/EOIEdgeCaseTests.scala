package xyz.hyperreal.char_reader

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Testing._

class EOIEdgeCaseTests extends AnyFreeSpec with Matchers {

  "single line indent, space, then empty" in {
    indent("1\n a \n  ") shouldBe
      """|<1> (line 1, column 1):
         |1
         |^
         |
         |<\n> (line 1, column 2):
         |1
         | ^
         |
         |<2, 2, INDENT>
         |<a> (line 2, column 2):
         | a !
         | ^
         |
         |< > (line 2, column 3):
         | a !
         |  ^
         |
         |<\n> (line 2, column 4):
         | a !
         |   ^
         |
         |<4, 1, DEDENT>
      """.trim.replace("!\n", "\n").stripMargin
  }

  "single line indent, then empty" in {
    indent("1\n a\n  ") shouldBe
      """|<1> (line 1, column 1):
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
         |<4, 1, DEDENT>
      """.trim.replace("!\n", "\n").stripMargin
  }

  "single line indent, then empty with nl" in {
    indent("1\n a\n  \n") shouldBe
      """|<1> (line 1, column 1):
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
         |<4, 1, DEDENT>
      """.trim.replace("!\n", "\n").stripMargin
  }

}
