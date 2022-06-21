package ass03.root

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.cluster.typed.Cluster
import com.typesafe.config.ConfigFactory
import ass03.sensors.{Sensor, ZoneLeader}
import ass03.firestation.FireStation

object RootLeader:
  def apply(i: Int): Behavior[Nothing] =
    Behaviors.setup {
      ctx =>
        val cluster = Cluster(ctx.system)
        if cluster.selfMember.hasRole("Sensor") then
          val s = ctx.spawn(Sensor(i), "Sensor")
        else
          val l = ctx.spawn(ZoneLeader(i), "Leader")
        Behaviors.empty
    }

object RootSensor:
  def apply(i: Int): Behavior[Nothing] =
    Behaviors.setup {
      ctx =>
        val cluster = Cluster(ctx.system)
        if cluster.selfMember.hasRole("Sensor") then
          val s = ctx.spawn(Sensor(i), "Sensor")
        else
          val l = ctx.spawn(ZoneLeader(i), "Leader")
        Behaviors.empty
    }


object RootFirestation:
  def apply(i: Int): Behavior[Nothing] =
    Behaviors.setup {
      ctx =>
        val cluster = Cluster(ctx.system)
        val l = ctx.spawn(FireStation(i), "FireStation")
        Behaviors.empty
    }