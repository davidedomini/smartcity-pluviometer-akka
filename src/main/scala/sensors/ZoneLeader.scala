package sensors

import msg.Message
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.receptionist.ServiceKey

object ZoneLeader:

  sealed trait Command extends Message
  case object Start extends Command
  case object PingSensor extends Command
  case object NewData extends Command
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
          case PingSensor =>
            println("ZONE LEADER => sensors: " + sensors)
            for
              s <- sensors
            yield s ! Sensor.Greet
            Behaviors.same
          case NewData =>
            println("LEADER => Il sensore mi ha inviato un nuovo dato ")
            Behaviors.same
          case TellMeYourZone(replyTo) =>
            println("MANDO RISPOSTA AL SENSORE")
            replyTo ! Sensor.ZoneOfTheLeader(zone, ctx.self)
            Behaviors.same
        }
    }
