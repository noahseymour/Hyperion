package Server

import java.io.{BufferedWriter, OutputStreamWriter}
import java.net.{ServerSocket, Socket}
import scala.io.Source

class Server(val port: Int, val backLog: Int = 1)

object Server {
  private val Response: String = "HTTP/1.0 200 OK\r\n\r\nHello World"

  def apply(port: Int): Server = new Server(port)

  def apply(port: Int, backLog: Int) = new Server(port, backLog)

  def run(server: Server): Unit = {
    println("SERVER:      Starting up...")
    val serverSocket: ServerSocket = ServerSocket(server.port, server.backLog)
    println(s"SERVER:      Listening for connections on port ${server.port}")
    while (true) {
      val client: Socket = serverSocket.accept()
      val request: String = Source.fromInputStream(client.getInputStream)
        .getLines().next()

      println(s"REQUEST:     $request")

      val writer = new BufferedWriter(new OutputStreamWriter(client.getOutputStream))
      writer.write(Response)
      writer.flush()

      println(s"RESPONSE:    $Response")

      client.close()
    }
    serverSocket.close()
  }
}
