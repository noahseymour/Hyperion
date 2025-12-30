package Server

import org.scalatest.funsuite.AnyFunSuite
import ParserCombinator.Parser
import HttpParser._

class HttpParserSuite extends AnyFunSuite {
  /* ---------- Helpers ---------- */

  private def succeeds[a](p: Parser[a], in: String): Boolean =
    p(in).isDefined

  private def succeedsWith[a](p: Parser[a], in: String, out: (a, String)): Boolean =
    p(in) contains out

  private def fails[a](p: Parser[a], in: String): Boolean =
    p(in).isEmpty


  /* ---------- crlf ---------- */

  test("crlf parses carriage-return line-feed") {
    assert(succeedsWith(crlf, "\r\nrest", ("\r\n", "rest")))
  }

  test("crlf fails if not exact") {
    assert(fails(crlf, "\n"))
    assert(fails(crlf, "\r"))
    assert(fails(crlf, "\r\r\n"))
  }


  /* ---------- tchar / token / method ---------- */

  test("tchar parses letters and digits") {
    assert(succeeds(tchar, "A"))
    assert(succeeds(tchar, "7"))
  }

  test("tchar parses allowed symbols") {
    "!#$%&'*+-.^_`|~".foreach { c =>
      assert(succeeds(tchar, c.toString))
    }
  }

  test("tchar rejects spaces and separators") {
    assert(fails(tchar, " "))
    assert(fails(tchar, "/"))
  }

  test("token parses multiple tchars") {
    assert(succeedsWith(token, "GET rest", ("GET", " rest")))
  }

  test("method parses HTTP methods") {
    assert(succeedsWith(method, "POST /", ("POST", " /")))
  }


  /* ---------- unreserved ---------- */

  test("unreserved parses alphanumerics") {
    assert(succeeds(unreserved, "a"))
    assert(succeeds(unreserved, "9"))
  }

  test("unreserved parses allowed symbols") {
    "-._~".foreach { c =>
      assert(succeeds(unreserved, c.toString))
    }
  }

  test("unreserved rejects reserved symbols") {
    assert(fails(unreserved, "/"))
    assert(fails(unreserved, "?"))
  }


  /* ---------- hex / pctEncoded ---------- */

  test("hex parses hexadecimal characters") {
    "0123456789abcdefABCDEF".foreach { c =>
      assert(succeeds(hex, c.toString))
    }
  }

  test("hex rejects non-hex characters") {
    assert(fails(hex, "g"))
    assert(fails(hex, "%"))
  }

  test("pctEncoded parses percent-encoded sequences") {
    assert(succeedsWith(pctEncoded, "%2Frest", ("%2F", "rest")))
  }

  test("pctEncoded fails on malformed encoding") {
    assert(fails(pctEncoded, "%"))
    assert(fails(pctEncoded, "%2"))
    assert(fails(pctEncoded, "%ZZ"))
  }


  /* ---------- pchar ---------- */

  test("pchar parses unreserved characters") {
    assert(succeedsWith(pchar, "a", ("a", "")))
  }

  test("pchar parses sub-delims") {
    "!$&'()*+,;=".foreach { c =>
      assert(succeedsWith(pchar, c.toString, (c.toString, "")))
    }
  }

  test("pchar parses : and @") {
    assert(succeedsWith(pchar, ":", (":", "")))
    assert(succeedsWith(pchar, "@", ("@", "")))
  }

  test("pchar parses percent-encoded values") {
    assert(succeedsWith(pchar, "%20", ("%20", "")))
  }


  /* ---------- segment ---------- */

  test("segment parses empty segment") {
    assert(succeedsWith(segment, "/rest", ("", "/rest")))
  }

  test("segment parses multiple pchars") {
    assert(succeedsWith(segment, "abc%20def/rest",
      ("abc%20def", "/rest")))
  }


  /* ---------- absPath ---------- */

  test("absPath parses single-segment absolute path") {
    assert(succeedsWith(absPath, "/abc",
      (List("abc"), "")))
  }

  test("absPath parses multi-segment path") {
    assert(succeedsWith(absPath, "/a/b/c",
      (List("a", "b", "c"), "")))
  }

  test("absPath allows empty segments") {
    assert(succeedsWith(absPath, "//a",
      (List("", "a"), "")))
  }


  /* ---------- query ---------- */

  test("query parses simple query string") {
    assert(succeedsWith(query, "a=b&c=d rest",
      ("a=b&c=d", " rest")))
  }

  test("query parses percent-encoded values") {
    assert(succeedsWith(query, "%20%2F",
      ("%20%2F", "")))
  }

  test("query may be empty") {
    assert(succeedsWith(query, "", ("", "")))
  }


  /* ---------- origin-form target ---------- */

  test("origin parses path without query") {
    assert(succeedsWith(origin, "/a/b",
      (Target.Origin(List("a", "b"), ""), "")))
  }

  test("origin parses path with query") {
    assert(succeedsWith(origin, "/a/b?x=1",
      (Target.Origin(List("a", "b"), "x=1"), "")))
  }

  test("origin rejects missing leading slash") {
    assert(fails(origin, "a/b"))
  }


  /* ---------- target ---------- */

  test("target selects origin form when applicable") {
    assert(succeeds(target, "/abc"))
  }


  /* ---------- HTTP version ---------- */

  test("version parses HTTP version correctly") {
    assert(succeedsWith(version, "HTTP/1.1",
      (Version(1, 1), "")))
  }

  test("version fails on malformed input") {
    assert(fails(version, "HTTP/"))
    assert(fails(version, "HTTP/1"))
    assert(fails(version, "HTTP/1."))
    assert(fails(version, "HTP/1.1"))
  }

}
