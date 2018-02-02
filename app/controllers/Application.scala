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

import scala.concurrent.duration._

/** Application controller, handles authentication */
class Application @Inject() (cached: Cached, val cache: AsyncCacheApi) extends Controller with Security with Logging {

  val cacheDuration = 1.day
  implicit val actorSystem = ActorSystem("ServiceName")
  implicit val materializer = ActorMaterializer()
  /**
   * Caching action that caches an OK response for the given amount of time with the key.
   * NotFound will be cached for 5 mins. Any other status will not be cached.
   */
//  def Caching(key: String, okDuration: Duration) =
//    new Cached(cache)
//      .status(_ => key, OK, okDuration.toSeconds.toInt)
//      .includeStatus(NOT_FOUND, 5.minutes.toSeconds.toInt)
  def caching(key: String) = cached
    .status(_ => key, OK)
    .includeStatus(NOT_FOUND)


  /** Serves the index page, see views/index.scala.html */
  def index(includeHeader: Boolean = true) = Action {
    log.debug("Index called")
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

  /** Used for obtaining the email and password from the HTTP login request */
  case class LoginCredentials(email: String, password: String)

  /** JSON reader for [[LoginCredentials]]. */
  implicit val LoginCredentialsFromJson = (
    (__ \ "email").read[String](minLength[String](5)) ~
      (__ \ "password").read[String](minLength[String](8))
    )((email, password) => LoginCredentials(email, password))

  /**
   * Log-in a user. Expects the credentials in the body in JSON format.
   *
   * Set the cookie [[AuthTokenCookieKey]] to have AngularJS set the X-XSRF-TOKEN in the HTTP
   * header.
   *
   * @return The token needed for subsequent requests
   */
  def login() = {
    val token = java.util.UUID.randomUUID.toString
    Ok(Json.obj("token" -> token))
      .withCookies(Cookie(AuthTokenCookieKey, token, None, httpOnly = false))
  }
//    Action(playBodyParsers.json) { implicit request =>
//    request.body.validate[LoginCredentials].fold(
//      errors => {
//        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))
//      },
//      credentials => {
//        // TODO Check credentials, log user in, return correct token
//        User.findByEmailAndPassword(credentials.email, credentials.password).fold {
//          log.info("Unregistered user tried to log in")
//          BadRequest(Json.obj("status" -> "KO", "message" -> "User not registered"))
//        } { user =>
//          /*
//           * For this demo, return a dummy token. A real application would require the following,
//           * as per the AngularJS documentation:
//           *
//           * > The token must be unique for each user and must be verifiable by the server (to
//           * > prevent the JavaScript from making up its own tokens). We recommend that the token is
//           * > a digest of your site's authentication cookie with a salt) for added security.
//           *
//           */
//          val token = java.util.UUID.randomUUID.toString
//          cache.set(token, user.id.get)
//          log.info(s"User ${user.id.get} succesfully logged in")
//          Ok(Json.obj("token" -> token))
//            .withCookies(Cookie(AuthTokenCookieKey, token, None, httpOnly = false))
//        }
//      }
//    )
//  }

  /**
   * Log-out a user. Invalidates the authentication token.
   *
   * Discard the cookie [[AuthTokenCookieKey]] to have AngularJS no longer set the
   * X-XSRF-TOKEN in HTTP header.
   */
  def logout() = {
//    HasToken(playBodyParsers.empty) { token => userId => implicit request =>
//    cache.remove(token)
//    log.info(s"User $userId succesfully logged out")
    Ok.discardingCookies(DiscardingCookie(name = AuthTokenCookieKey))
  }

  def options = Action {
    Ok("")
  }

}
