package Server

import ParserCombinator.*

private[Server] class HeaderField(val name: String, val value: String)

object HttpParser {
  private val TCharSymbols = "!#$%&'*+-.^_`|~"
  private val SubDelims = "!$&'()*+,;="
  private val PCharSpecialSymbols = ":@"
  private val QuerySpecialSymbols = "/?"
  private val HexSymbols = "0123456789abcdefABCDEF"
  private val UnreservedSymbols = "-._~"

  /* REQUEST-LINE PARSING. */

  private val space: Parser[Char] = sat(_ == ' ')
  private[Server] val crlf: Parser[String] = identifier("\r\n")

  private[Server] val TChar: Parser[Char] = sat(_.isLetterOrDigit) + sat(TCharSymbols.contains(_))
  private[Server] val token: Parser[String] = many1(TChar).map(_.mkString)
  private[Server] val method: Parser[String] = token

  private[Server] val unreserved: Parser[Char] = sat(_.isLetterOrDigit) + sat(UnreservedSymbols.contains(_))

  private[Server] val hex: Parser[Char] = sat(HexSymbols.contains(_))
  private[Server] val pctEncoded: Parser[String] = for {
    _ <- sat(_ == '%')
    d1 <- hex
    d2 <- hex
  } yield s"%$d1$d2"

  private[Server] val subDelims: Parser[Char] = sat(SubDelims.contains(_))

  private[Server] val PChar: Parser[String] = (unreserved + subDelims + sat(PCharSpecialSymbols.contains(_))).map(_.toString) + pctEncoded

  private[Server] val segment: Parser[String] = many(PChar).map(_.mkString)
  // TODO: sepBy works?
  private[Server] val absPath: Parser[List[String]] = startSepBy1(segment)(sat(_ == '/'))

  private[Server] val query: Parser[String] = many(PChar + sat(QuerySpecialSymbols.contains(_)).map(_.toString)).
    map(_.flatten.mkString)

  private[Server] val origin: Parser[Target] = for {
    p <- absPath
    q <- optional(for {
      _ <- identifier("?")
      q <- query
    } yield q)("")
  } yield Target.Origin(absPath = p, query = q)
  private[Server] val absolute: Parser[Target] = fail // TODO: needed?
  private[Server] val authority: Parser[Target] = fail // TODO: needed?
  private[Server] val asterisk: Parser[Target] = fail // TODO: needed?

  private[Server] val target: Parser[Target] = origin + absolute + authority + asterisk

  private[Server] val version: Parser[Version] = for {
    _ <- identifier("HTTP/")
    major <- sat(_.isDigit)
    _ <- identifier(".")
    minor <- sat(_.isDigit)
  } yield Version(major = Integer.parseInt(major.toString), minor = Integer.parseInt(minor.toString))

  /* HEADER FIELD PARSING. */

  private[Server] val fieldName: Parser[String] = token

  private[Server] val VChar: Parser[Char] = sat(x => x >= 33 && x <= 126)
  private[Server] val obsText: Parser[Char] = sat(x => x >= 128 && x <= 255)
  private[Server] val fieldVChar: Parser[Char] = VChar + obsText

  private[Server] val whitespace: Parser[Char] = space + sat(_ == '\t')

  private[Server] val fieldContent: Parser[String] = for {
    a <- fieldVChar
    b <- optional(for {
      ws <- many1(whitespace) // TODO: remove whitespace?
      v <- fieldVChar
    } yield ws.appended(v))(Nil)
  } yield (a :: b).mkString

  private[Server] val obsFold: Parser[String] = for {
    _ <- crlf
    ws <- many1(whitespace).map(_.toString)
  } yield "\r\n" + ws

  private[Server] val fieldValue: Parser[String] = many(fieldContent + obsFold).map(_.mkString)

  private[Server] val optionalWhitespace: Parser[String] = many(whitespace).map(_.mkString)
  private[Server] val headerField: Parser[HeaderField] = for {
    name <- fieldName
    _ <- identifier(":")
    _ <- optionalWhitespace
    value <- fieldValue
    _ <- optionalWhitespace
  } yield HeaderField(name = name, value = value)

  // TODO: sepBy works?
  private[Server] val headerFields: Parser[Map[String, String]] = sepBy(headerField)(crlf).map(
    _.foldLeft(Map.empty)((m, hf) => m + (hf.name -> hf.value))
  )

  private[Server] val body: Parser[String] = consume

  // Parses a HTTP request.
  val parse: Parser[HttpRequest] = for {
    // Start-line
    m <- method
    _ <- space
    t <- target
    _ <- space
    v <- version
    _ <- crlf

    // Header-fields
    hfs <- headerFields
    _ <- crlf
    b <- body
  } yield HttpRequest(method = m, target = t, version = v, headerFields = hfs, body = b)
}
