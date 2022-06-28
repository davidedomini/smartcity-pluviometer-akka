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
  case object GenerateData extends Command
  case class ZoneOfTheLeader(z: Int, l: ActorRef[ZoneLeader.Command]) extends Command
  case class WasLastDataAlarming(l: ActorRef[ZoneLeader.Command]) extends Command
  
  def apply(myZone: Int): Behavior[Command | Receptionist.Listing] =

    var leader: Option[ActorRef[ZoneLeader.Command]] = Option.empty

    Behaviors.setup[Command | Receptionist.Listing] { ctx =>

      ctx.system.receptionist ! Receptionist.Subscribe(ZoneLeader.Service, ctx.self)

      Behaviors.withTimers {
        timers =>
          // Sends a generate message every ten seconds
          timers.startTimerAtFixedRate(GenerateData, 10000 milliseconds)
          SensorLogic(myZone, ctx.self, Option.empty, 0)
      }

    }

  def SensorLogic(
         myZone: Int,
         myself: ActorRef[Command],
         myLeader: Option[ActorRef[ZoneLeader.Command]],
         lastValue: Double
  ): Behavior[Command | Receptionist.Listing] =
    Behaviors.receiveMessage {
          
      case GenerateData =>
        val r = Random.between(0.0, 1.0)
        if r > 0.85 && !myLeader.isEmpty then 
          myLeader.get ! ZoneLeader.PingAlarm
        SensorLogic(myZone, myself, myLeader, r)

      case msg: Receptionist.Listing =>
        if(myLeader.isEmpty) then
          val leaders = msg.serviceInstances(ZoneLeader.Service).toList
          for
            l <- leaders
          yield l ! ZoneLeader.TellMeYourZone(myself)
        SensorLogic(myZone, myself, myLeader, lastValue)

      case ZoneOfTheLeader(z, l) =>
        if z == myZone then
          l ! ZoneLeader.RegistrySensor(myself)
          SensorLogic(myZone, myself, Option(l), lastValue)
        else
          SensorLogic(myZone, myself, myLeader, lastValue)

      case WasLastDataAlarming(replyTo) => 
        val wasAlarming: Boolean =  lastValue > 0.85
        replyTo ! ZoneLeader.LastData(wasAlarming)
        SensorLogic(myZone, myself, myLeader, lastValue)
    }
