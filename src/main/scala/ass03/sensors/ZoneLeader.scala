package ass03.sensors

import ass03.msg.Message
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.receptionist.ServiceKey

object ZoneLeader:

  sealed trait Command extends Message
  case object Start extends Command
  case object PingAlarm extends Command
  case class RegistrySensor(s: ActorRef[Sensor.Command]) extends Command
  case class TellMeYourZone(replyTo: ActorRef[Sensor.Command]) extends Command

  val Service = ServiceKey[Command]("Leader")

  def apply(zone: Int): Behavior[Command] =
    var sensors : List[ActorRef[Sensor.Command]] = List.empty
    Behaviors.setup[Command]{
      ctx =>
        ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)

        Behaviors.receiveMessage {
          case Start =>
            println("Leader partito")
            Behaviors.same

          case PingAlarm =>
            println("LEADER => Il sensore mi ha inviato un nuovo dato ")
            Behaviors.same

          case TellMeYourZone(replyTo) =>
            println("MANDO RISPOSTA AL SENSORE")
            replyTo ! Sensor.ZoneOfTheLeader(zone, ctx.self)
            Behaviors.same

          case RegistrySensor(s) =>
            sensors = sensors :+ s
            Behaviors.same
        }
    }
