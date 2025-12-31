package Server

enum Target {
  case Origin(absPath: List[String], query: String)
  case Absolute
  case Authority
  case Asterisk
}

class Version(val major: Int, val minor: Int)

class HttpRequest(val method: String, 
                  val target: Target,
                  val version: Version,
                  val headerFields: Map[String, String],
                  val body: String) // TODO: string the best option here?
