package sensors

import msg.Message
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import concurrent.duration.DurationInt
import scala.language.postfixOps

import akka.actor.typed.receptionist.ServiceKey
//Il sensore parte e si registra

object Sensor:
  sealed trait Command extends Message
  case object Start extends Command
  case object Greet extends Command
  case object GenerateData extends Command

  val Service = ServiceKey[Command]("Sensor")

  def apply(i: Int): Behavior[Command] =

    Behaviors.setup { ctx =>
      ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
      Behaviors.withTimers {
        timers =>
          timers.startTimerAtFixedRate(GenerateData, 1000 milliseconds) // ogni secondo si manda un messaggio di generate
          SensorLogic()
      }
//      Behaviors.receiveMessage {
//        case Start =>
//          println("Sensore partito " + i)
//          Behaviors.same
//
//        case Greet =>
//          println("Ciao sono il sensore " + i)
//          Behaviors.same
//      }
    }

  def SensorLogic(): Behavior[Command] =
    Behaviors.receiveMessage {
      case Start =>
        println("Sensore partito ")
        SensorLogic()

      case Greet =>
        println("Ciao sono il sensore ")
        SensorLogic()

      case GenerateData =>
        println("Sto generandooooooo")
        SensorLogic()

    }
