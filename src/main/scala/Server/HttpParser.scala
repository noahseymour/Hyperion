package Server

import ParserCombinator._

object HttpParser {
  def singleSpace: Parser[Char] = sat(_ == ' ')
  def tchar: Parser[Char] = sat(_.isLetterOrDigit) + sat("!#$%&'*+-.^_`|~".contains(_))
  
  def token: Parser[String] = ???
}
