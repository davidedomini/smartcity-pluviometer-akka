package ass03.firestation

import ass03.firestation.Gui
import ass03.msg.Message
import ass03.{Zone, CityParams}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import ass03.sensors.{Sensor, ZoneLeader}

object FireStation:
  sealed trait Command extends Message
  case class Alarm(zone: Int) extends Command
  case class ZoneOfTheLeader(z: Int, l: ActorRef[ZoneLeader.Command]) extends Command
  
  def apply(zones: List[Zone], myZone: Int, w: CityParams): Behavior[Command | Receptionist.Listing] =

    var leader: Option[ActorRef[ZoneLeader.Command]] = Option.empty
    val view = Gui(w.width, w.height, myZone)

    Behaviors.setup[Command | Receptionist.Listing] { ctx =>
      println("FIRE STATION: " + zones)
      ctx.system.receptionist ! Receptionist.Subscribe(ZoneLeader.Service, ctx.self)
      val situation = zones.map(z => (z, false))
      view.render(situation)
      FireStationLogic(myZone, ctx.self, Option.empty, view, situation)
    }

  def FireStationLogic(
          myZone: Int,
          mySelf: ActorRef[Command],
          leader: Option[ActorRef[ZoneLeader.Command]],
          view: Gui,
          situation: List[(Zone, Boolean)]
  ): Behavior[Command | Receptionist.Listing] =
    Behaviors.receiveMessage {
      case msg: Receptionist.Listing =>
        if(leader.isEmpty) then
          val leaders = msg.serviceInstances(ZoneLeader.Service).toList
          for
            l <- leaders
          yield l ! ZoneLeader.TellMeYourZoneFirestation(mySelf)
        FireStationLogic(myZone, mySelf, leader, view, situation)

      case ZoneOfTheLeader(z, l) =>
        if z == myZone && leader.isEmpty then
          println("FIRE STATION " + mySelf + "il leader della mia zona è " + l)
          l ! ZoneLeader.RegistryFirestation(mySelf)
          FireStationLogic(myZone, mySelf, Option(l), view, situation)
        else
          FireStationLogic(myZone, mySelf, leader, view, situation)

      case Alarm(zone) =>
        view.render(situation.map(s => if s._1.index == zone then (s._1, true) else s))
        FireStationLogic(myZone, mySelf, leader, view, situation)
    }