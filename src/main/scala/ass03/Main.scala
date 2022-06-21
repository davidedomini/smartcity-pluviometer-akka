package ass03

import ass03.Main.{assignSensorsToZone, computeZone}
import ass03.root.{RootLeader, RootSensor, RootFirestation}

case class CityParams(width: Int, height: Int, rows: Int, columns: Int, numberOfSensors: Int, alarmTheshold: Int)
case class Zone(index: Int, x: Int, offsetX: Int, y: Int, offsetY: Int)


object Main extends App:

  val world = CityParams(600, 200, 2, 3, 20, 31)
  val zones = computeZone(world)
  println(zones)
  val sz = assignSensorsToZone(world) //Coppia(sensore, zona)
  println(sz)

//  for
//    z <- zones
//  yield launchLeader1(z)
//
//  for
//    s <- sz
//    sensor = s._1
//    zone = s._2
//  yield deploySensor(zone)

  def deployFireStation(z: Int): Unit =
    val port = 8280 + z
    startupWithRole("FireStation", port)(RootFirestation(zones.toList, z, world))

  def deploySensor(z: (Int, Int)): Unit =
    val port = 8080 + z._1
    startupWithRole("Sensor", port)(RootSensor(z._2))

  def deployLeader(zone: Zone): Unit =
    val port = 2550 + zone.index
    startupWithRole("Leader", port)(RootLeader(zone.index))

  def computeZone(w: CityParams): Seq[Zone] =
    val offsetY = w.height / w.rows
    val offsetX = w.width / w.columns
    for
      i <- 0 to (w.rows - 1)
      j <- 0 to (w.columns - 1)
      x = offsetX * j
      y = offsetY * i
      index = (j + 1) + (i * w.rows) + (i)
    yield Zone(index, x, offsetX, y, offsetY)

  def assignSensorsToZone(w: CityParams): List[(Int, Int)] =
    val numberOfZone = w.columns * w.rows
    val sensorsPerZone = Math.ceil((w.numberOfSensors / numberOfZone)).toInt
    println(sensorsPerZone)
    //TODO è troppo imperativo trovare un modo funzionale
    var sz: List[(Int, Int)]= List.empty
    var j = 0
    var z = 1
    for
      i <- 0 to w.numberOfSensors
    do
      sz = sz :+ (i, z)
      j = j + 1
      if j == sensorsPerZone && z < numberOfZone then
        j = 0
        z = z +1
    sz


  //LEADER
  @main def launchLeader1(): Unit =
    deployLeader(zones.toList.apply(0))

  @main def launchLeader2(): Unit =
    deployLeader(zones.toList.apply(1))

  @main def launchLeader3(): Unit =
    deployLeader(zones.toList.apply(2))

  @main def launchLeader4(): Unit =
    deployLeader(zones.toList.apply(3))

  @main def launchLeader5(): Unit =
    deployLeader(zones.toList.apply(4))

  @main def launchLeader6(): Unit =
    deployLeader(zones.toList.apply(5))


  //FIRESTATION

  @main def launchFirestation1(): Unit =
    deployFireStation(1)

  @main def launchFirestation2(): Unit =
    deployFireStation(2)

  //SENSORI
  @main def launchSensor1(): Unit =
    deploySensor(sz.toList.apply(0))

  @main def launchSensor2(): Unit =
    deploySensor(sz.toList.apply(1))

  @main def launchSensor3(): Unit =
    deploySensor(sz.toList.apply(2))

  @main def launchSensor4(): Unit =
    deploySensor(sz.toList.apply(3))

  @main def launchSensor5(): Unit =
    deploySensor(sz.toList.apply(4))

  @main def launchSensor6(): Unit =
    deploySensor(sz.toList.apply(5))

  @main def launchSensor7(): Unit =
    deploySensor(sz.toList.apply(6))

  @main def launchSensor8(): Unit =
    deploySensor(sz.toList.apply(7))

  @main def launchSensor9(): Unit =
    deploySensor(sz.toList.apply(8))

  @main def launchSensor10(): Unit =
    deploySensor(sz.toList.apply(9))

  @main def launchSensor11(): Unit =
    deploySensor(sz.toList.apply(10))

  @main def launchSensor12(): Unit =
    deploySensor(sz.toList.apply(11))

  @main def launchSensor13(): Unit =
    deploySensor(sz.toList.apply(12))

  @main def launchSensor14(): Unit =
    deploySensor(sz.toList.apply(13))

  @main def launchSensor15(): Unit =
    deploySensor(sz.toList.apply(14))

  @main def launchSensor16(): Unit =
    deploySensor(sz.toList.apply(15))

  @main def launchSensor17(): Unit =
    deploySensor(sz.toList.apply(16))

  @main def launchSensor18(): Unit =
    deploySensor(sz.toList.apply(17))

  @main def launchSensor19(): Unit =
    deploySensor(sz.toList.apply(18))

  @main def launchSensor20(): Unit =
    deploySensor(sz.toList.apply(19))


// Potenzialmente da cancellare
object MainSensor extends App:
  println("ciao")
  startupWithRole("Sensor", 8081)(RootSensor(1))

object MainLeader1 extends App:
  startupWithRole("Leader", 2551)(RootLeader(1))
  startupWithRole("Leader", 8082)(RootLeader(2))

object MainLeader2 extends App:
  startupWithRole("Leader", 2551)(RootLeader(1))