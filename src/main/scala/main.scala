import java.io.{BufferedWriter, OutputStreamWriter}
import java.net.ServerSocket
import java.net.Socket
import scala.io.Source

val ServerPort: Int = 8080
val BackLog: Int = 1
val Response: String = "HTTP/1.0 200 OK\r\n\r\nHello World"

@main
def main(): Unit = {
  // Creates a TCP socket, binds it to port 8080, and listens for connections.
  val serverSocket: ServerSocket = ServerSocket(ServerPort, BackLog)

  println("Server listening on localhost port 80...")

  while (true) {
    val client: Socket = serverSocket.accept()
    val request: String = Source.fromInputStream(client.getInputStream)
      .getLines()
      .takeWhile(_.nonEmpty)
      .mkString("\n")

    println(request)

    val writer = new BufferedWriter(new OutputStreamWriter(client.getOutputStream))
    writer.write(Response)
    writer.flush()

    client.close()
  }

  serverSocket.close()

  println("Server shut down.")
}
