package util

import akka.actor.ActorSystem

trait Logging {
  val actorSystem: ActorSystem

  val logging = akka.event.Logging(actorSystem,
    this.getClass.getCanonicalName)
}
