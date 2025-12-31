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
    assert(succeeds(TChar, "A"))
    assert(succeeds(TChar, "7"))
  }

  test("tchar parses allowed symbols") {
    "!#$%&'*+-.^_`|~".foreach { c =>
      assert(succeeds(TChar, c.toString))
    }
  }

  test("tchar rejects spaces and separators") {
    assert(fails(TChar, " "))
    assert(fails(TChar, "/"))
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
    assert(succeedsWith(PChar, "a", ("a", "")))
  }

  test("pchar parses sub-delims") {
    "!$&'()*+,;=".foreach { c =>
      assert(succeedsWith(PChar, c.toString, (c.toString, "")))
    }
  }

  test("pchar parses : and @") {
    assert(succeedsWith(PChar, ":", (":", "")))
    assert(succeedsWith(PChar, "@", ("@", "")))
  }

  test("pchar parses percent-encoded values") {
    assert(succeedsWith(PChar, "%20", ("%20", "")))
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

  // TODO: why does this fail?
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


  /* ---------- field-name ---------- */

  test("fieldName parses valid token") {
    assert(succeedsWith(fieldName, "Host:", ("Host", ":")))
  }

  test("fieldName rejects invalid characters") {
    assert(fails(fieldName, "Ho st"))
    assert(fails(fieldName, "Host/"))
  }


  /* ---------- whitespace ---------- */

  test("whitespace parses space and tab") {
    assert(succeeds(whitespace, " "))
    assert(succeeds(whitespace, "\t"))
  }

  test("whitespace rejects other characters") {
    assert(fails(whitespace, "a"))
  }


  /* ---------- field-content ---------- */

  test("fieldContent parses visible characters") {
    assert(succeedsWith(fieldContent, "abc", ("abc", "")))
  }

  test("fieldContent allows internal whitespace") {
    assert(succeedsWith(fieldContent, "a b", ("a b", "")))
  }

  test("fieldContent rejects leading whitespace") {
    assert(fails(fieldContent, " abc"))
  }


  /* ---------- obs-fold ---------- */

  test("obsFold parses folded header continuation") {
    assert(succeedsWith(obsFold, "\r\n value", ("\r\n value", "")))
  }

  test("obsFold fails without whitespace after CRLF") {
    assert(fails(obsFold, "\r\nx"))
  }


  /* ---------- field-value ---------- */

  test("fieldValue parses simple value") {
    assert(succeedsWith(fieldValue, "text/plain", ("text/plain", "")))
  }

  test("fieldValue parses folded values") {
    val input = "abc\r\n def"
    assert(succeedsWith(fieldValue, input, ("abc\r\n def", "")))
  }

  test("fieldValue allows empty value") {
    assert(succeedsWith(fieldValue, "", ("", "")))
  }


  /* ---------- header-field ---------- */

  test("headerField parses simple header") {
    val input = "Host: example.com"
    val out = HeaderField("Host", "example.com")
    assert(succeedsWith(headerField, input, (out, "")))
  }

  test("headerField allows optional whitespace after colon") {
    val input = "Host:\t example.com"
    val out = HeaderField("Host", "example.com")
    assert(succeedsWith(headerField, input, (out, "")))
  }

  test("headerField trims trailing whitespace") {
    val input = "Host: example.com   "
    val out = HeaderField("Host", "example.com")
    assert(succeedsWith(headerField, input, (out, "")))
  }

  test("headerField fails without colon") {
    assert(fails(headerField, "Host example.com"))
  }


  /* ---------- headerFields ---------- */

  test("headerFields parses single header") {
    val input =
      "Host: example.com\r\n"

    assert(succeedsWith(
      headerFields,
      input,
      (Map("Host" -> "example.com"), "")
    ))
  }

  test("headerFields parses multiple headers") {
    val input =
      "Host: example.com\r\n" +
        "Content-Type: text/plain\r\n"

    assert(succeedsWith(
      headerFields,
      input,
      (Map(
        "Host" -> "example.com",
        "Content-Type" -> "text/plain"
      ), "")
    ))
  }

  test("headerFields allows folded headers") {
    val input =
      "X-Test: abc\r\n" +
        " def\r\n"

    assert(succeedsWith(
      headerFields,
      input,
      (Map("X-Test" -> "abc\r\n def"), "")
    ))
  }

  test("headerFields fails without terminating CRLF") {
    val input = "Host: example.com"
    assert(fails(headerFields, input))
  }


  /* ---------- body ---------- */

  test("body consumes entire remaining input") {
    assert(succeedsWith(body, "hello world", ("hello world", "")))
  }

  test("body may be empty") {
    assert(succeedsWith(body, "", ("", "")))
  }


  /* ---------- full request (sanity check) ---------- */

  test("parse full HTTP request with headers and body") {
    val input =
      "GET /path HTTP/1.1\r\n" +
        "Host: example.com\r\n" +
        "Content-Type: text/plain\r\n" +
        "\r\n" +
        "hello"

    assert(succeeds(parse, input))
  }

  test("parse full HTTP request with query, folded headers, and body") {
    val input =
      "POST /a/b/c?x=1&y=2 HTTP/1.1\r\n" +
        "Host: example.com\r\n" +
        "X-Long-Header: first-part\r\n" +
        "\tsecond-part\r\n" +
        "Content-Type: text/plain\r\n" +
        "\r\n" +
        "This is the body.\nWith multiple lines."

    val expected =
      HttpRequest(
        method = "POST",
        target = Target.Origin(
          absPath = List("a", "b", "c"),
          query = "x=1&y=2"
        ),
        version = Version(1, 1),
        headerFields = Map(
          "Host" -> "example.com",
          "X-Long-Header" -> "first-part\r\n\tsecond-part",
          "Content-Type" -> "text/plain"
        ),
        body = "This is the body.\nWith multiple lines."
      )

    assert(succeedsWith(parse, input, (expected, "")))
  }
}
