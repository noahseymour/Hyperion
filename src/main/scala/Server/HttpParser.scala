package Server

import ParserCombinator.*

object HttpParser {
  private val TCharSymbols = "!#$%&'*+-.^_`|~"
  private val SubDelims = "!$&'()*+,;="
  private val PCharSpecialSymbols = ":@"
  private val QuerySpecialSymbols = "/?"
  private val HexSymbols = "0123456789abcdefABCDEF"
  private val UnreservedSymbols = "-._~"

  private val singleSpace: Parser[Char] = sat(_ == ' ')
  private[Server] val crlf: Parser[String] = identifier("\r\n")

  private[Server] val tchar: Parser[Char] = sat(_.isLetterOrDigit) + sat(TCharSymbols.contains(_))
  private[Server] val token: Parser[String] = many1(tchar).map(_.mkString)
  private[Server] val method: Parser[String] = token

  private[Server] val unreserved: Parser[Char] = sat(_.isLetterOrDigit) + sat(UnreservedSymbols.contains(_))

  private[Server] val hex: Parser[Char] = sat(HexSymbols.contains(_))
  private[Server] val pctEncoded: Parser[String] = for {
    _ <- sat(_ == '%')
    d1 <- hex
    d2 <- hex
  } yield s"%$d1$d2"

  private[Server] val subDelims: Parser[Char] = sat(SubDelims.contains(_))

  private[Server] val pchar: Parser[String] = (unreserved + subDelims + sat(PCharSpecialSymbols.contains(_))).map(_.toString) + pctEncoded

  private[Server] val segment: Parser[String] = many(pchar).map(_.foldLeft("")(_ ++ _))
  private[Server] val absPath: Parser[List[String]] = startSepBy1(segment)(sat(_ == '/'))

  private[Server] val query: Parser[String] = many(pchar + sat(QuerySpecialSymbols.contains(_)).map(_.toString)).map(_.foldLeft("")(_ ++ _))

  private[Server] val origin: Parser[Target] = for {
    p <- absPath
    q <- optional(for {
      _ <- identifier("?")
      q <- query
    } yield q)("")
  } yield Target.Origin(absPath = p, query = q)
  private[Server] val absolute: Parser[Target] = fail // TODO: complete
  private[Server] val authority: Parser[Target] = fail // TODO: complete
  private[Server] val asterisk: Parser[Target] = fail // TODO: complete

  private[Server] val target: Parser[Target] = origin + absolute + authority + asterisk

  private[Server] val version: Parser[Version] = for {
    _ <- identifier("HTTP/")
    major <- sat(_.isDigit)
    _ <- identifier(".")
    minor <- sat(_.isDigit)
  } yield Version(major = Integer.parseInt(major.toString), minor = Integer.parseInt(minor.toString))
}
