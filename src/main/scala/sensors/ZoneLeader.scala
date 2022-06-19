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

  //val Service = ServiceKey[Command]("Leader")

  def apply(): Behavior[Command | Receptionist.Listing] =
    var sensors : List[ActorRef[Sensor.Command]] = List.empty
    Behaviors.setup[Command | Receptionist.Listing]{
      ctx =>

        ctx.system.receptionist ! Receptionist.Subscribe(Sensor.Service, ctx.self)
        Behaviors.receiveMessage {
          case msg: Receptionist.Listing =>
            println("ZONE LEADER => Nuovo sensore registrato ")
            sensors = msg.serviceInstances(Sensor.Service).toList
            println("ZONE LEADER => sensors: " + sensors)
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
          case NewData =>
            println("LEADER => Il sensore mi ha inviato un nuovo dato ")
            Behaviors.same
        }
    }
