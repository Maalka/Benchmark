package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

//import models.User

/** Example controller; see conf/routes for the the mapping to routes */
object Users extends Controller with Security {

  /** Retrieves the user for the given id as JSON */
  def user(id: Long) = Action(parse.empty) { request =>
    // TODO Find user and convert to JSON
    Ok(Json.obj("firstName" -> "John", "lastName" -> "Smith", "age" -> 42))
  }

  /** Creates a user from the given JSON */
  def createUser() = Action(parse.json) { request =>
    // TODO Implement User creation, typically via request.body.validate[User]
    Ok
  }

  /** Updates the user for the given id from the JSON body */
  def updateUser(id: Long) = Action(parse.json) { request =>
    // TODO Implement User creation, typically via request.body.validate[User]
    Ok
  }

  /** Deletes a user for the given id */
  def deleteUser(id: Long) = Action(parse.empty) { request =>
    // TODO Implement User creation, typically via request.body.validate[User]
    Ok
  }

}
