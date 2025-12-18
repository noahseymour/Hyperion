package Server

import ParserCombinator._

object HttpParser {
  def singleSpace: Parser[Char] = sat(_ == ' ')
  def tchar: Parser[Char] = ???
  def token: Parser[String] = ???
}
