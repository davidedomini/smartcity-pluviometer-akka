import root.Root


case class SimulationParams(width: Int, height: Int, rows: Int, columns: Int, numberOfSensors: Int, alarmTheshold: Int)
case class Zone(index: Int, x: Int, offsetX: Int, y: Int, offsetY: Int)

object Main extends App:

  val world = SimulationParams(600, 200, 2, 3, 10, 31)
  println("ciao")
  val zones = computeZone(world)
  println(zones)

  def deploySensor(zone: Int): Unit =
    val port = 8080 + zone
    startupWithRole("Sensor", port)(Root(zone))

  def deployLeader(zone: Int): Unit =
    val port = 2550 + zone
    startupWithRole("Leader", port)(Root(zone))

  def computeZone(w: SimulationParams): Seq[Zone] =
    val offsetY = w.height / w.rows
    val offsetX = w.width / w.columns
    for
      i <- 0 to (w.rows - 1)
      j <- 0 to (w.columns - 1)
      x = offsetX * j
      y = offsetY * i
      index = (j + 1) + (i * w.rows) + (i)
    yield Zone(index, x, offsetX, y, offsetY)


// Potenzialmente da cancellare
object MainSensor extends App:
  println("ciao")
  startupWithRole("Sensor", 8081)(Root(1))

object MainLeader1 extends App:
  startupWithRole("Leader", 2551)(Root(1))
  startupWithRole("Leader", 8082)(Root(2))

object MainLeader2 extends App:
  startupWithRole("Leader", 2551)(Root(1))