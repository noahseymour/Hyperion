import Server.Server

@main
def main(): Unit = {
  val myServer: Server = Server(port = 80, backLog = 10)

  Server.run(myServer)
}
