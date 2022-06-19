package sensors

import msg.Message
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

object ZoneLeader:

  sealed trait Command extends Message
  case object Start extends Command
  case object PingSensor extends Command

  def apply(): Behavior[Command | Receptionist.Listing] =
    var sensors : List[ActorRef[Sensor.Command]] = List.empty
    Behaviors.setup[Command | Receptionist.Listing]{
      ctx =>
        ctx.system.receptionist ! Receptionist.Subscribe(Sensor.Service, ctx.self)
        Behaviors.receiveMessage {
          case msg: Receptionist.Listing =>
            sensors = msg.serviceInstances(Sensor.Service).toList
            Behaviors.same
          case Start =>
            println("Leader partito")
            Behaviors.same
          case PingSensor =>
            println("ZONE LEADER => sensors: " + sensors)
            for
              s <- sensors
            yield s ! Sensor.Greet
            Behaviors.same
        }
    }
