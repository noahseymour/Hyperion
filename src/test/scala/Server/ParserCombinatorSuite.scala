package Server

import org.scalatest.funsuite.AnyFunSuite
import ParserCombinator._

class ParserCombinatorSuite extends AnyFunSuite {

  val Number: String = "123456"
  val Word: String = "Hello, world!"

  test("twoDigit correct for valid string") {
    assert(twoDigit(Number).contains(("12", "3456")))
  }

  test("twoDigit does not parse word") {
    assert(twoDigit(Word).isEmpty)
  }

  test("word correct for valid string") {
    assert(word(Word).contains((List('H', 'e', 'l', 'l', 'o'), ", world!")))
  }

  test("word does not parse digits") {
    assert(word(Number).isEmpty)
  }
}
