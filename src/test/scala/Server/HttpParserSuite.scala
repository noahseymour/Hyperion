package Server

import org.scalatest.funsuite.AnyFunSuite
import HttpParser._

class HttpParserSuite extends AnyFunSuite {
  val getMethod = "GET"
  val postMethod = "POST"
  val getRequestTarget = "/where?q=now"
  val postRequestTarget = "/api/v1/users"
  val httpVersion = "HTTP/1.1"
  val crlf = "\r\n"
}
