package xyz.hyperreal.char_reader

import org.scalatest._
import freespec.AnyFreeSpec
import matchers.should.Matchers

import Testing._

class SimpleTests extends AnyFreeSpec with Matchers {

  "empty" in {
    indent("") shouldBe ""
  }

  "single char" in {
    indent("1") shouldBe
      """
        |<1> (line 1, column 1):
        |1
        |^
        |""".trim.stripMargin
  }

  "char, space" in {
    indent("1 ") shouldBe
      """
        |<1> (line 1, column 1):
        |1 !
        |^
        |
        |< > (line 1, column 2):
        |1 !
        | ^
        |""".trim.replace("!\n", "\n").stripMargin
  }

  "char, nl" in {
    indent("1\n") shouldBe
      """
        |<1> (line 1, column 1):
        |1
        |^
        |
        |<\n> (line 1, column 2):
        |1
        | ^
        |""".trim.replace("!\n", "\n").stripMargin
  }

  "char, space, nl" in {
    indent("1 \n") shouldBe
      """
        |<1> (line 1, column 1):
        |1 !
        |^
        |
        |< > (line 1, column 2):
        |1 !
        | ^
        |
        |<\n> (line 1, column 3):
        |1 !
        |  ^
        |""".trim.replace("!\n", "\n").stripMargin
  }

  "char, nl, space" in {
    indent("1\n ") shouldBe
      """
        |<1> (line 1, column 1):
        |1
        |^
        |
        |<\n> (line 1, column 2):
        |1
        | ^
        |""".trim.replace("!\n", "\n").stripMargin
  }

  "char, nl, space (noindent)" in {
    noindent("1\n ") shouldBe
      """
        |<1> (line 1, column 1):
        |1
        |^
        |
        |<\n> (line 1, column 2):
        |1
        | ^
        |
        |< > (line 2, column 1):
        | !
        |^
        |""".trim.replace("!\n", "\n").stripMargin
  }

  "char, space, nl, space" in {
    indent("1 \n ") shouldBe
      """
        |<1> (line 1, column 1):
        |1 !
        |^
        |
        |< > (line 1, column 2):
        |1 !
        | ^
        |
        |<\n> (line 1, column 3):
        |1 !
        |  ^
        |""".trim.replace("!\n", "\n").stripMargin
  }

  "char, space, nl, space (noindent)" in {
    noindent("1 \n ") shouldBe
      """
        |<1> (line 1, column 1):
        |1 !
        |^
        |
        |< > (line 1, column 2):
        |1 !
        | ^
        |
        |<\n> (line 1, column 3):
        |1 !
        |  ^
        |
        |< > (line 2, column 1):
        | !
        |^
        |""".trim.replace("!\n", "\n").stripMargin
  }

}
