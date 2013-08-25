package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

/** Example controller */
object Users extends Controller {

  /** Retrieves the user for the given id as JSON */
  def user(id: Long) = Action {
    // TODO Implement this correctly
    Ok(Json.obj("firstName" -> "John", "lastName" -> "Smith", "age" -> 42))
  }

}
