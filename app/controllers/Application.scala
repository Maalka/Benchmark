package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

/** Application controller, handles authentication */
object Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }

  def jsRoutes(varName: String = "jsRoutes") = Action { implicit request =>
    Ok(
      Routes.javascriptRouter(varName)(
        routes.javascript.Application.login,
        routes.javascript.Application.logout,
        routes.javascript.Users.user
        // TODO Add your routes here
      )
    ).as(JAVASCRIPT)
  }

  def login() = Action(parse.json) { implicit request =>
    // TODO Check credentials, log user in, return correct token
    Ok(Json.obj("token" -> java.util.UUID.randomUUID().toString))
  }

  def logout() = Action {
    // TODO Invalidate token
    Ok
  }

}
