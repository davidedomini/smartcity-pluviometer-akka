package ass03.sensors

import ass03.msg.Message
import ass03.firestation.FireStation
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.receptionist.ServiceKey
import akka.cluster.typed.{Cluster, Subscribe}
import akka.cluster.ClusterEvent.{MemberEvent, MemberExited}

object ZoneLeader:

  sealed trait Command extends Message
  case object PingAlarm extends Command
  case class RegistrySensor(s: ActorRef[Sensor.Command]) extends Command
  case class RegistryFirestation(s: ActorRef[FireStation.Command]) extends Command
  case class TellMeYourZone(replyTo: ActorRef[Sensor.Command]) extends Command
  case class GetStatus(replyTo: ActorRef[FireStation.Command]) extends Command
  case class TellMeYourZoneFirestation(replyTo: ActorRef[FireStation.Command]) extends Command
  case class AlarmUnderManagement(s: ActorRef[FireStation.Command]) extends Command
  case class AlarmResolved(s: ActorRef[FireStation.Command]) extends Command
  case class LastData(wasAlarming: Boolean) extends Command

  val Service = ServiceKey[Command]("Leader")

  def apply(zone: Int): Behavior[Command | MemberEvent] =
    var sensors : List[ActorRef[Sensor.Command]] = List.empty
    var status: String = "NoAlarm"
    var fireStation: Option[ActorRef[FireStation.Command]] = Option.empty
    var majority: Int = 0
    var alarms: Int = 0

    Behaviors.setup[Command | MemberEvent]{
      ctx =>
        ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
        val cluster = Cluster(ctx.system)
        cluster.subscriptions ! Subscribe(ctx.self, classOf[MemberExited])

        Behaviors.receiveMessage {
          case msg: MemberEvent =>
            val s = msg.member.address
            val sensorAddresses = sensors.map(e => e.path.address)
            if sensorAddresses.contains(s) then
              println("Rimuovo sensore " + s)
              sensors = sensors.filter( e => !e.path.address.equals(s) )
              println(sensors)
              majority = Math.ceil(sensors.length / 2).toInt
            Behaviors.same

          case PingAlarm => 
            println("LEADER => Il sensore mi ha inviato un nuovo dato di allarme")
            alarms = 0
            for
              s <- sensors
            yield s ! Sensor.WasLastDataAlarming(ctx.self)
            Behaviors.same

          case TellMeYourZone(replyTo) =>
            replyTo ! Sensor.ZoneOfTheLeader(zone, ctx.self)
            Behaviors.same

          case TellMeYourZoneFirestation(replyTo) =>
            replyTo ! FireStation.ZoneOfTheLeader(zone, ctx.self)
            Behaviors.same

          case RegistrySensor(s) =>
            sensors = sensors :+ s
            majority = Math.ceil(sensors.length / 2).toInt + 1
            Behaviors.same

          case RegistryFirestation(fs) =>
            fireStation = Option(fs)
            Behaviors.same

          case GetStatus(replyTo) =>
            replyTo ! FireStation.ZoneStatus(zone, status, sensors.length, fireStation)
            Behaviors.same

          case AlarmUnderManagement(s) =>
            if s == fireStation.get && status.equals("Alarm") then
              status = "UnderManagement"
            Behaviors.same

          case AlarmResolved(s) =>
            if s == fireStation.get then
              status = "NoAlarm"
            Behaviors.same

          case LastData(w: Boolean) =>
            if w then
              alarms = alarms + 1
            if alarms >= majority then
              if !fireStation.isEmpty && status.matches("NoAlarm") then
                status = "Alarm"
                fireStation.get ! FireStation.Alarm(zone)
            Behaviors.same
        }
    }
