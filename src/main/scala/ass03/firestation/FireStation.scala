package ass03.firestation
import ass03.msg.Message
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import ass03.sensors.{Sensor, ZoneLeader}

object FireStation:
  sealed trait Command extends Message
  case class Alarm(zone: Int) extends Command
  case class ZoneOfTheLeader(z: Int, l: ActorRef[ZoneLeader.Command]) extends Command


  def apply(myZone: Int): Behavior[Command | Receptionist.Listing] =

    var leader: Option[ActorRef[ZoneLeader.Command]] = Option.empty

    Behaviors.setup[Command | Receptionist.Listing] { ctx =>

      ctx.system.receptionist ! Receptionist.Subscribe(ZoneLeader.Service, ctx.self)

      FireStationLogic(myZone, ctx.self, Option.empty)
    }


  def FireStationLogic(
          myZone: Int,
          mySelf: ActorRef[Command],
          leader: Option[ActorRef[ZoneLeader.Command]]
  ): Behavior[Command | Receptionist.Listing] =
    Behaviors.receiveMessage {
      case msg: Receptionist.Listing =>
        if(leader.isEmpty) then
          val leaders = msg.serviceInstances(ZoneLeader.Service).toList
          for
            l <- leaders
          yield l ! ZoneLeader.TellMeYourZoneFirestation(mySelf)
        FireStationLogic(myZone, mySelf, leader)

      case ZoneOfTheLeader(z, l) =>
        if z == myZone && leader.isEmpty then
          println("FIRE STATION " + mySelf + "il leader della mia zona è " + l)
          FireStationLogic(myZone, mySelf, Option(l))
        else
          FireStationLogic(myZone, mySelf, leader)


    }