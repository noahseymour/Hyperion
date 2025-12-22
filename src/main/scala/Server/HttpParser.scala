package Server

import ParserCombinator.*

enum RequestTarget {
  case OriginForm(absPath: List[String], query: String)
}

class Version(major: Int, minor: Int)

enum HttpRequest {
  case RequestLine(method: String, requestTarget: RequestTarget, version: Version)
}

object HttpParser {
  private val SubDelims: String = "!$&'()*+,;="

  private val singleSpace: Parser[Char] = sat(_ == ' ')
  private val crlf: Parser[String] = for {
    _ <- sat(_ == '\r')
    _ <- sat(_ == '\n')
  } yield "\r\n"

  private val tchar: Parser[Char] = sat(_.isLetterOrDigit) + sat("!#$%&'*+-.^_`|~".contains(_))
  private val token: Parser[String] = many1(tchar).map(_.mkString)

  private val unreserved: Parser[String] = (sat(_.isLetterOrDigit) + sat("-._~".contains(_))).map(_.toString)
  private val hex: Parser[Char] = sat("0123456789abcdefABCDEF".contains(_))
  // TODO: Ths is probably wrong
  private val pctEncoded: Parser[String] = for {
    _ <- sat(_ == '%')
    d1 <- hex
    d2 <- hex
  } yield s"%$d1$d2"
  private val subDelims: Parser[String] = sat(SubDelims.contains(_)).map(_.toString)
  private val pchar: Parser[String] = unreserved + pctEncoded + subDelims + sat(":@".contains(_)).map(_.toString)
  private val segment: Parser[String] = many(pchar).map(_.mkString)
  private val absPath: Parser[List[String]] = startSepBy1(segment)(sat(_ == '/'))
  private val query: Parser[String] = pchar + sat("/?".contains(_)).map(_.toString)


  private val originForm: Parser[RequestTarget] = for {
    path <- absPath
    q <- optional(for {
      _ <- sat(_ == '?')
      q <- query
    } yield q)("")
  } yield RequestTarget.OriginForm(absPath = path, query = q)
  private val absoluteForm: Parser[RequestTarget] = ???
  private val authorityForm: Parser[RequestTarget] = ???
  private val asteriskForm: Parser[RequestTarget] = ???


  private val requestTarget: Parser[RequestTarget] = originForm + absoluteForm + authorityForm + asteriskForm


  private val version: Parser[Version] = for {
    _ <- identifier("HTTP")
    _ <- sat(_ == '/')
    major <- sat(_.isDigit)
    _ <- sat(_ == '.')
    minor <- sat(_.isDigit)
  } yield Version(major = major, minor = minor)

  val requestLine: Parser[HttpRequest] = for {
    m <- token
    _ <- singleSpace
    rt <- requestTarget
    _ <- singleSpace
    v <- version
    _ <- crlf
  } yield HttpRequest.RequestLine(method = m, requestTarget = rt, version = v)
}
