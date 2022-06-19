package sensors

import msg.Message
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

import akka.actor.typed.receptionist.ServiceKey
//Il sensore parte e si registra

object Sensor:
  sealed trait Command extends Message
  case object Start extends Command
  case object Greet extends Command
  val Service = ServiceKey[Command]("Sensor")

  def apply(i: Int): Behavior[Command] =

    Behaviors.setup { ctx =>
      ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
      Behaviors.receiveMessage {
        case Start =>
          println("Sensore partito " + i)
          Behaviors.same

        case Greet =>
          println("Ciao sono il sensore " + i)
          Behaviors.same
      }
    }
