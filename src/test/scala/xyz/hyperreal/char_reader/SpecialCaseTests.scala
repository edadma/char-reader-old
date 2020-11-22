package xyz.hyperreal.char_reader

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import xyz.hyperreal.char_reader.Testing._

class SpecialCaseTests extends AnyFreeSpec with Matchers {

  "indented text" in {
    text(
      """|1
         | a
         |  b
         | c
         |2
         | d
         |""".stripMargin
    ) shouldBe
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
         |< > (line 3, column 2):
         |  b
         | ^
         |
         |<b> (line 3, column 3):
         |  b
         |  ^
         |
         |<\n> (line 3, column 4):
         |  b
         |   ^
         |
         |<c> (line 4, column 2):
         | c
         | ^
         |
         |<\n> (line 4, column 3):
         | c
         |  ^
         |
         |<5, 1, DEDENT>
         |<2> (line 5, column 1):
         |2
         |^
         |
         |<\n> (line 5, column 2):
         |2
         | ^
         |
         |<6, 2, INDENT>
         |<d> (line 6, column 2):
         | d
         | ^
         |
         |<\n> (line 6, column 3):
         | d
         |  ^
         |
         |<7, 1, DEDENT>
      """.trim.replace("!\n", "\n").stripMargin
  }

}
