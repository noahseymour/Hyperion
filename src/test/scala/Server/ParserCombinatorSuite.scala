package Server

import org.scalatest.funsuite.AnyFunSuite
import ParserCombinator._

class ParserCombinatorSuite extends AnyFunSuite {
  /* ---------- Helpers ---------- */

  private def succeeds[A](p: Parser[A], in: String): Boolean =
    p(in).isDefined

  private def succeedsWith[A](p: Parser[A], in: String, out: (A, String)): Boolean =
    p(in).contains(out)

  private def fails[A](p: Parser[A], in: String): Boolean =
    p(in).isEmpty


  /* ---------- item ---------- */

  test("item consumes exactly one character") {
    assert(succeedsWith(char, "abc", ('a', "bc")))
  }

  test("item fails on empty input") {
    assert(fails(char, ""))
  }


  /* ---------- sat ---------- */

  test("sat succeeds when predicate holds") {
    val p = sat(_.isLetter)
    assert(succeedsWith(p, "abc", ('a', "bc")))
  }

  test("sat fails when predicate does not hold") {
    val p = sat(_.isDigit)
    assert(fails(p, "abc"))
  }

  test("sat fails on empty input") {
    val p = sat(_ == 'a')
    assert(fails(p, ""))
  }


  /* ---------- ignore ---------- */

  test("ignore returns value without consuming input") {
    val p = ParserCombinator.ignore(42)
    assert(succeedsWith(p, "abc", (42, "abc")))
  }


  /* ---------- map (Functor behaviour) ---------- */

  test("map transforms the parsed value") {
    val p = sat(_.isDigit).map(_.asDigit)
    assert(succeedsWith(p, "7abc", (7, "abc")))
  }

  test("map does not affect input consumption") {
    val p = sat(_.isLower).map(_.toUpper)
    assert(succeedsWith(p, "abc", ('A', "bc")))
  }


  /* ---------- flatMap (Monad behaviour) ---------- */

  test("flatMap sequences parsers correctly") {
    val p = for {
      x <- sat(_.isLetter)
      y <- sat(_.isLetter)
    } yield s"$x$y"

    assert(succeedsWith(p, "abc", ("ab", "c")))
  }

  test("flatMap propagates failure") {
    val p = for {
      _ <- sat(_.isDigit)
      _ <- sat(_.isLetter)
    } yield ()

    assert(fails(p, "abc"))
  }


  /* ---------- withFilter ---------- */

  test("withFilter allows filtering parsed values") {
    val p = for {
      x <- sat(_.isDigit)
      if x.asDigit > 5
    } yield x

    assert(succeedsWith(p, "7abc", ('7', "abc")))
    assert(fails(p, "3abc"))
  }


  /* ---------- alternative (+) ---------- */

  test("+ tries left parser first") {
    val p = sat(_ == 'a') + sat(_ == 'b')
    assert(succeedsWith(p, "abc", ('a', "bc")))
  }

  test("+ tries right parser if left fails") {
    val p = sat(_ == 'a') + sat(_ == 'b')
    assert(succeedsWith(p, "bcd", ('b', "cd")))
  }

  test("+ fails if both alternatives fail") {
    val p = sat(_ == 'a') + sat(_ == 'b')
    assert(fails(p, "xyz"))
  }


  /* ---------- optional ---------- */

  test("optional returns parsed value when parser succeeds") {
    val p = optional(sat(_.isDigit))('x')
    assert(succeedsWith(p, "7abc", ('7', "abc")))
  }

  test("optional returns default without consuming input when parser fails") {
    val p = optional(sat(_.isDigit))('x')
    assert(succeedsWith(p, "abc", ('x', "abc")))
  }


  /* ---------- many / many1 ---------- */

  test("many parses zero occurrences") {
    val p = many(sat(_.isDigit))
    assert(succeedsWith(p, "abc", (Nil, "abc")))
  }

  test("many parses multiple occurrences") {
    val p = many(sat(_.isDigit))
    assert(succeedsWith(p, "123abc", (List('1', '2', '3'), "abc")))
  }

  test("many never fails") {
    val p = many(sat(_ == 'x'))
    assert(succeeds(p, "abc"))
  }

  test("many1 parses one or more occurrences") {
    val p = many1(sat(_.isDigit))
    assert(succeedsWith(p, "7abc", (List('7'), "abc")))
  }

  test("many1 fails on zero occurrences") {
    val p = many1(sat(_.isDigit))
    assert(fails(p, "abc"))
  }


  /* ---------- sepBy / sepBy1 ---------- */

  test("sepBy parses zero elements") {
    val p = sepBy(sat(_.isDigit))(sat(_ == ','))
    assert(succeedsWith(p, "abc", (Nil, "abc")))
  }

  test("sepBy parses separated elements") {
    val p = sepBy(sat(_.isDigit))(sat(_ == ','))
    assert(succeedsWith(p, "1,2,3abc", (List('1', '2', '3'), "abc")))
  }

  test("sepBy1 fails on zero elements") {
    val p = sepBy1(sat(_.isDigit))(sat(_ == ','))
    assert(fails(p, "abc"))
  }


  /* ---------- startSepBy ---------- */

  test("startSepBy parses nothing if separator absent") {
    val p = startSepBy(sat(_.isDigit))(sat(_ == ','))
    assert(succeedsWith(p, "abc", (Nil, "abc")))
  }

  test("startSepBy parses leading-separated elements") {
    val p = startSepBy(sat(_.isDigit))(sat(_ == ','))
    assert(succeedsWith(p, ",1,2abc", (List('1', '2'), "abc")))
  }


  /* ---------- identifier ---------- */

  test("identifier matches exact string") {
    val p = identifier("hello")
    assert(succeedsWith(p, "hello world", ("hello", " world")))
  }

  test("identifier fails on mismatch") {
    val p = identifier("hello")
    assert(fails(p, "hellx"))
    assert(fails(p, ""))
  }

  test("identifier with empty string always succeeds") {
    val p = identifier("")
    assert(succeedsWith(p, "abc", ("", "abc")))
  }


  /* ---------- realistic composition ---------- */

  test("parsing a two-digit number") {
    assert(succeedsWith(twoDigit, "42xyz", ("42", "xyz")))
  }

  test("word parses only letters") {
    assert(succeedsWith(word, "hello123",
      (List('h', 'e', 'l', 'l', 'o'), "123")))
  }

}
