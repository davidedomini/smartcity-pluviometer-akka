package ass03.firestation
import ass03.msg.Message
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import ass03.sensors.ZoneLeader

object FireStation:
  sealed trait Command extends Message
  case class Alarm(zone: Int) extends Command
  case class ZoneOfTheLeader(z: Int, l: ActorRef[ZoneLeader.Command]) extends Command