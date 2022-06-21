package ass03.firestation

import ass03.firestation.Gui
import ass03.msg.Message
import ass03.{CityParams, Zone}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import ass03.sensors.{Sensor, ZoneLeader}

import concurrent.duration.DurationInt
import scala.language.postfixOps

object FireStation:
  sealed trait Command extends Message
  case object RequireStatus extends Command
  case object ManageAlarm extends Command
  case object ResolveAlarm extends Command
  case class Alarm(zone: Int) extends Command
  case class ZoneOfTheLeader(z: Int, l: ActorRef[ZoneLeader.Command]) extends Command
  case class ZoneStatus(zone: Int, status: String) extends Command
  
  def apply(zones: List[Zone], myZone: Int, w: CityParams): Behavior[Command | Receptionist.Listing] =

    var leader: Option[ActorRef[ZoneLeader.Command]] = Option.empty


    Behaviors.setup[Command | Receptionist.Listing] { ctx =>
      println("FIRE STATION: " + zones)
      val view = Gui(w.width, w.height, myZone, ctx.self)
      ctx.system.receptionist ! Receptionist.Subscribe(ZoneLeader.Service, ctx.self)
      val situation = zones.map(z => (z, "NoAlarm"))
      view.render(situation)

      Behaviors.withTimers{ timers =>
        timers.startTimerAtFixedRate(RequireStatus, 5000 milliseconds)
        FireStationLogic(myZone, ctx.self, Option.empty, List.empty, view, situation)
      }
    }

  def FireStationLogic(
          myZone: Int,
          mySelf: ActorRef[Command],
          leaderOfMyZone: Option[ActorRef[ZoneLeader.Command]],
          leaders: List[ActorRef[ZoneLeader.Command]],
          view: Gui,
          situation: List[(Zone, String)]
  ): Behavior[Command | Receptionist.Listing] =
    Behaviors.receiveMessage {

      case msg: Receptionist.Listing =>
        val leaders = msg.serviceInstances(ZoneLeader.Service).toList
        if(leaderOfMyZone.isEmpty) then
          //val leaders = msg.serviceInstances(ZoneLeader.Service).toList
          for
            l <- leaders
          yield l ! ZoneLeader.TellMeYourZoneFirestation(mySelf)
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, view, situation)

      case ZoneOfTheLeader(z, l) =>
        if z == myZone && leaderOfMyZone.isEmpty then
          println("FIRE STATION " + mySelf + "il leader della mia zona è " + l)
          l ! ZoneLeader.RegistryFirestation(mySelf)
          FireStationLogic(myZone, mySelf, Option(l), leaders, view, situation)
        else
          FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, view, situation)

      case Alarm(zone) =>
        view.render(situation.map(s => if s._1.index == zone then (s._1, "Alarm") else s))
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, view, situation)

      case RequireStatus =>
        println("FIRESTATION: " + myZone + "RICHIEDO STATUS AI LEADER, conosco i leader" + leaders)
        for
          l <- leaders
        yield l ! ZoneLeader.GetStatus(mySelf)
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, view, situation)

      case ZoneStatus(z, s) =>
        //Cambiare la situazione della zona z in s
        val newSituation = situation.map( e => if e._1.index == z then (e._1, s) else e )
        view.render(newSituation)
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, view, newSituation)

      case ManageAlarm => ???

      case ResolveAlarm => ???
    }