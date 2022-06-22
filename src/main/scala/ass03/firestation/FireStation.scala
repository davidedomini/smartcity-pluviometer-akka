package ass03.firestation

import ass03.firestation.Gui
import ass03.msg.Message
import ass03.{CityParams, Zone}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.cluster.ClusterEvent.{MemberEvent, MemberUp}
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import ass03.sensors.Sensor.Start
import ass03.sensors.{Sensor, ZoneLeader}
import akka.cluster.typed.{Cluster, Subscribe}
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
  case class FirestationStatus(zone: Int, status: String) extends Command
  case class StationStatus(zone: Int, status: String) extends Command
  case object RequireFirestationStatus extends Command 
  case class DiscoverFirestaions(nZone: Int) extends Command
  case class ZoneFirestation(f: ActorRef[Command]) extends Command
  case class GetStatus(replyTo: ActorRef[Command]) extends Command

  val ServiceFirestation = ServiceKey[Command]("Firestation")

  def apply(zones: List[Zone], myZone: Int, w: CityParams): Behavior[Command | Receptionist.Listing | MemberEvent] =

    var leader: Option[ActorRef[ZoneLeader.Command]] = Option.empty


    Behaviors.setup[Command | Receptionist.Listing| MemberEvent] { ctx =>
      println("FIRE STATION: " + zones)
      val view = Gui(w.width, w.height, myZone, ctx.self)

      //ctx.system.receptionist ! Receptionist.Register(ServiceFirestation, ctx.self)
      ctx.system.receptionist ! Receptionist.Subscribe(ZoneLeader.Service, ctx.self)
      //ctx.system.receptionist ! Receptionist.Subscribe(ServiceFirestation, ctx.self)

      val cluster = Cluster(ctx.system)
      cluster.subscriptions ! Subscribe(ctx.self, classOf[MemberUp])

      val situation = zones.map(z => (z, "NoAlarm"))
      view.render(situation)

      //val nz = w.columns * w.rows
      //ctx.self ! DiscoverFirestaions(nz)

      Behaviors.withTimers{ timers =>
        timers.startTimerAtFixedRate(RequireStatus, 5000 milliseconds)
        timers.startTimerAtFixedRate(RequireFirestationStatus, 5000 milliseconds)
        FireStationLogic(myZone, ctx.self, Option.empty, List.empty, List.empty, view, situation, "Free")
      }

    }

  def FireStationLogic(
          myZone: Int,
          mySelf: ActorRef[Command],
          leaderOfMyZone: Option[ActorRef[ZoneLeader.Command]],
          leaders: List[ActorRef[ZoneLeader.Command]],
          stations: List[ActorRef[Command]],
          view: Gui,
          situation: List[(Zone, String)],
          status: String
  ): Behavior[Command | Receptionist.Listing | MemberEvent] =
    Behaviors.receiveMessage {

      case msg: MemberEvent =>
        println("FIRESTATION RECEIVED EVENT:" + msg)
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, status)

      case msg: Receptionist.Listing =>
        val leaders = msg.serviceInstances(ZoneLeader.Service).toList
        //val stations = msg.serviceInstances(ServiceFirestation).toList
        val stations = List.empty
        println("STAZIONE DEI POMPIERI, le altre stazioni sono: " + stations)

        if(leaderOfMyZone.isEmpty) then
          for
            l <- leaders
          yield l ! ZoneLeader.TellMeYourZoneFirestation(mySelf)
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, status)

      case ZoneOfTheLeader(z, l) =>
        if z == myZone && leaderOfMyZone.isEmpty then
          println("FIRE STATION " + mySelf + "il leader della mia zona è " + l)
          l ! ZoneLeader.RegistryFirestation(mySelf)
          FireStationLogic(myZone, mySelf, Option(l), leaders, stations, view, situation, status)
        else
          FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, status)

      case Alarm(zone) =>
        view.render(situation.map(s => if s._1.index == zone then (s._1, "Alarm") else s))
        if zone == myZone then
          FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, "Busy")
        else
          FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, status)

      case RequireStatus =>
        //println("FIRESTATION: " + myZone + "RICHIEDO STATUS AI LEADER, conosco i leader" + leaders)
        for
          l <- leaders
        yield l ! ZoneLeader.GetStatus(mySelf)
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, status)

      case ZoneStatus(z, s) =>
        //Cambia la situazione della zona z in s
        val newSituation = situation.map( e => if e._1.index == z then (e._1, s) else e )
        view.render(newSituation)
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, newSituation, status)

      case ManageAlarm =>
        leaderOfMyZone.get ! ZoneLeader.AlarmUnderManagement(mySelf)
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, status)

      case ResolveAlarm =>
        leaderOfMyZone.get ! ZoneLeader.AlarmResolved(mySelf)
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, "Free")

      case DiscoverFirestaions(nz) =>
        if stations.length < nz then
          for
            l <- leaders
          yield l ! ZoneLeader.WhoIsYourFirestation(mySelf)
          mySelf ! DiscoverFirestaions(nz +  1)
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, status)

      case ZoneFirestation(f) =>
        if !stations.contains(f) && !f.equals(mySelf) then
          val newStations = stations :+ f
          println("Le stazioni sono: " + newStations)
          FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, newStations, view, situation, status)
        else
          FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, status)

      case RequireFirestationStatus =>
        for 
          s <- stations
        yield s ! GetStatus(mySelf)
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, status)

      case GetStatus(replyTo) =>
        replyTo ! FirestationStatus(myZone, status)
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, status)

      case FirestationStatus(z, s) =>
        view.updateStationsStatus((z,s))
        FireStationLogic(myZone, mySelf, leaderOfMyZone, leaders, stations, view, situation, status)
    }