import root.Root

object MainSensor extends App:
  println("ciao")
  startupWithRole("Sensor", 8081)(Root(1))

object MainLeader1 extends App:
  startupWithRole("Leader", 2551)(Root(1))
  startupWithRole("Leader", 8082)(Root(2))

object MainLeader2 extends App:
  startupWithRole("Leader", 2551)(Root(1))