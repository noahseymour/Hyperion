package Server

enum Method {
  case GET
  case HEAD
}

enum Target {
  case Origin(absPath: List[String], query: String)
  case Absolute
  case Authority
  case Asterisk
}

class Version(val major: Int, val minor: Int)

class HttpRequest(val method: Method, 
                  val target: Target,
                  val version: Version,
                  val headerFields: Map[Unit, Unit], // TODO: create header field type
                  val body: String) // TODO: string the best option here?
