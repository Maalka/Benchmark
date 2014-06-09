package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.cache._

/** Application controller, handles authentication */
object Application extends Controller with Security {

  /** Serves the index page, see views/index.scala.html */
  def index = Action {
    Ok(views.html.index())
  }

  /**
   * Returns the JavaScript router that the client can use for "type-safe" routes.
   * Uses browser caching; set duration (in seconds) according to your release cycle.
   * @param varName The name of the global variable, defaults to `jsRoutes`
   */
  def jsRoutes(varName: String = "jsRoutes") = Cached(_ => "jsRoutes", duration = 86400) {
    Action { implicit request =>
      Ok(
        Routes.javascriptRouter(varName)(
          routes.javascript.Application.login,
          routes.javascript.Application.logout,
          routes.javascript.Users.user,
          routes.javascript.Users.createUser,
          routes.javascript.Users.updateUser,
          routes.javascript.Users.deleteUser
          // TODO Add your routes here
        )
      ).as(JAVASCRIPT)
    }
  }

  /**
    * Log-in a user. Pass the credentials as JSON body.
    *
    * Set the cookie {@link AuthTokenCookieKey} to have AngularJS set X-XSRF-TOKEN in the HTTP
    * header.
    *
    * @return The token needed for subsequent requests
    */
  def login() = Action(parse.json) { implicit request =>
    // TODO Check credentials, log user in, return correct token
    //
    // For this demo, pretend user with ID 3 logged in
    val token = java.util.UUID.randomUUID().toString
    val userId = 3L;
    Cache.set(token, userId)
    Ok(Json.obj("token" -> token))
      .withCookies(Cookie(AuthTokenCookieKey, token, None, httpOnly = false))
  }

  /**
    * Logs the user out, i.e. with the token invalidated.
    *
    * Discard the cookie {@link AuthTokenCookieKey} to have AngularJS no longer set the
    * X-XSRF-TOKEN in HTTP header.
    */
  def logout() = HasToken(parse.json) { token => userId => implicit request =>
    Logger.info(s"logging out: token: $token")
    Cache.remove(token)
    Ok.discardingCookies(DiscardingCookie(name = AuthTokenCookieKey))
  }

}
