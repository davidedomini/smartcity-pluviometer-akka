package ass03.sensors

import ass03.msg.Message
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

import concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.Random

object Sensor:
  sealed trait Command extends Message
  case object Start extends Command
  case object GenerateData extends Command
  case class ZoneOfTheLeader(z: Int, l: ActorRef[ZoneLeader.Command]) extends Command
  
  def apply(i: Int): Behavior[Command | Receptionist.Listing] =

    var leader: Option[ActorRef[ZoneLeader.Command]] = Option.empty

    Behaviors.setup[Command | Receptionist.Listing] { ctx =>

      ctx.system.receptionist ! Receptionist.Subscribe(ZoneLeader.Service, ctx.self)

      Behaviors.withTimers {
        timers =>
          // ogni secondo si manda un messaggio di generate
          timers.startTimerAtFixedRate(GenerateData, 1000 milliseconds)
          SensorLogic(i, ctx.self, Option.empty)
      }

    }

  def SensorLogic(
         myZone: Int,
         myself: ActorRef[Command],
         myLeader: Option[ActorRef[ZoneLeader.Command]]
  ): Behavior[Command | Receptionist.Listing] =
    Behaviors.receiveMessage {
      case Start =>
        println("Sensore partito ")
        SensorLogic(myZone, myself, myLeader)

      case GenerateData =>
        val r = Random.between(0.0, 1.0)
        println("Ho generato: "+ r)
        if r > 0.9 && !myLeader.isEmpty then 
          myLeader.get ! ZoneLeader.PingAlarm
        SensorLogic(myZone, myself, myLeader)

      case msg: Receptionist.Listing =>
        if(myLeader.isEmpty) then
          val leaders = msg.serviceInstances(ZoneLeader.Service).toList
          for
            l <- leaders
          yield l ! ZoneLeader.TellMeYourZone(myself)
        SensorLogic(myZone, myself, myLeader)

      case ZoneOfTheLeader(z, l) =>
        if z == myZone then
          println("SENSORE " + myself + " => Il mio leader è: " + l)
          SensorLogic(myZone, myself, Option(l))
        else
          SensorLogic(myZone, myself, myLeader)
    }
