package root

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.cluster.typed.Cluster
import com.typesafe.config.ConfigFactory
import sensors.{Sensor, ZoneLeader}

object Root:
  def apply(i: Int): Behavior[Nothing] =
    Behaviors.setup {
      ctx =>
        val cluster = Cluster(ctx.system)
        if cluster.selfMember.hasRole("Sensor") then
          val s = ctx.spawn(Sensor(i), "Sensor")
          //s ! Sensor.Start
        else
          val l = ctx.spawn(ZoneLeader(i), "Leader")
          //l ! ZoneLeader.Start
          //l ! ZoneLeader.PingSensor
          //l ! ZoneLeader.PingSensor
          //l ! ZoneLeader.PingSensor
        Behaviors.empty
    }
