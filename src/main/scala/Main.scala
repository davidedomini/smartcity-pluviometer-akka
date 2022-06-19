import root.Root

object MainSensor extends App:
  println("ciao")
  startupWithRole("Sensor", 8081)(Root())

object MainLeader extends App:
  startupWithRole("Leader", 8082)(Root())