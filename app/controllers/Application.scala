package controllers

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import models._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc._
import com.google.inject.Inject
import play.api.cache.{AsyncCacheApi, Cached, SyncCacheApi}

import _root_.util.Logging

import scala.concurrent.duration._

/** Application controller, handles authentication */
class Application @Inject() (
                              cached: Cached,
                              val cache: AsyncCacheApi,
                              cc: ControllerComponents
                            )(
                            implicit val actorSystem: ActorSystem) extends AbstractController(cc) with Logging {



  val cacheDuration = 1.day

  implicit val materializer = ActorMaterializer()

  /**
   * Caching action that caches an OK response for the given amount of time with the key.
   * NotFound will be cached for 5 mins. Any other status will not be cached.
   */

  def caching(key: String) = cached
    .status(_ => key, OK)
    .includeStatus(NOT_FOUND)

  /** Serves the index page, see views/index.scala.html */
  def index(includeHeader: Boolean = true) = Action {
    logging.debug("Index called")
    Ok(views.html.index(includeHeader))
  }

  /**
   * Retrieves all routes via reflection.
   * http://stackoverflow.com/questions/12012703/less-verbose-way-of-generating-play-2s-javascript-router
   * @todo If you have controllers in multiple packages, you need to add each package here.
   */
  val routeCache = {
    val jsRoutesClasses = Seq(classOf[routes.javascript]) // TODO add your own packages
    jsRoutesClasses.flatMap { jsRoutesClass =>
      val controllers = jsRoutesClass.getFields.map(_.get(null))
      controllers.flatMap { controller =>
        controller.getClass.getDeclaredMethods.filter(_.getName != "_defaultPrefix").map { action =>
          action.invoke(controller).asInstanceOf[play.api.routing.JavaScriptReverseRoute]
        }
      }
    }
  }

  /**
   * Returns the JavaScript router that the client can use for "type-safe" routes.
   * Uses browser caching; set duration (in seconds) according to your release cycle.
   * @param varName The name of the global variable, defaults to `jsRoutes`
   */
  def jsRoutes(varName: String = "jsRoutes") = caching("jsRoutes") {
    Action { implicit request =>
      Ok(play.api.routing.JavaScriptReverseRouter(varName)(routeCache: _*)).as(JAVASCRIPT)
    }
  }

  def options = Action {
    Ok("")
  }

}
